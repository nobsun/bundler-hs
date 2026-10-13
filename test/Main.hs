module Main (main) where

import Bundler (bundle)
import Bundler.Config
  ( Config (..),
    EmbedPosition (..),
    FormatMode (..),
    noMinify,
    noTreeShake,
    parseConfigFromArgs,
  )
import Bundler.Error (BundleError (..), renderBundleError)
import Bundler.Ghc (TargetGhc (..), builtinGhc, glasgowHaskell, parsePreludeExports)
import Control.Exception (bracket_)
import Control.Monad (filterM, when)
import Data.ByteString.Lazy qualified as LBS
import Data.List (isInfixOf, isPrefixOf, sort)
import Data.Maybe (fromMaybe, isJust)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import GHC.IO.Encoding (char8, getLocaleEncoding, setLocaleEncoding)
import System.Directory
  ( createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    getTemporaryDirectory,
    listDirectory,
    removeDirectoryRecursive,
    removeFile,
  )
import System.Environment (getEnv, lookupEnv, setEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((<.>), (</>))
import System.IO (hClose, hPutStr, hSetEncoding, openTempFile, utf8)
import System.Process (readProcessWithExitCode)
import Test.Tasty (TestTree, defaultMain, localOption, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)
import Test.Tasty.Runners (NumThreads (..))

fixturesRoot :: FilePath
fixturesRoot = "test" </> "fixtures"

-- | A stand-in compiler answering from a recorded Prelude interface, so
-- that no golden depends on whichever ghc is on PATH.
fakeGhc :: String -> FilePath
fakeGhc version = "test" </> "fake-ghc" </> version </> "ghc"

pinnedGhc :: FilePath
pinnedGhc = fakeGhc "9.10.3"

main :: IO ()
main = do
  compileGate <- isJust <$> lookupEnv "HSB_TEST_COMPILE"
  entries <- sort <$> listDirectory fixturesRoot
  dirs <- filterM (doesDirectoryExist . (fixturesRoot </>)) entries
  tests <- traverse (fixtureTest compileGate) dirs
  -- The unit tests below mutate process-global state (TMPDIR, the locale
  -- encoding), so nothing may run beside them.
  defaultMain
    ( localOption
        (NumThreads 1)
        ( testGroup
            "all"
            [ testGroup "golden" tests,
              testGroup
                "unit"
                [ formatFailureSalvage,
                  utf8UnderNonUtf8Locale,
                  builtinPreludeMatchesGhc,
                  ghcVersionMacro,
                  missingGhcFallsBack,
                  unusableExplicitGhcFails
                ]
            ]
        )
    )

-- | A failing formatter must not lose the bundle: the pre-format output
-- is saved to a temp file whose content matches the --no-format output
-- exactly.
formatFailureSalvage :: TestTree
formatFailureSalvage = testCase "format failure saves the unformatted bundle" $ do
  systemTmp <- getTemporaryDirectory
  let sandbox = systemTmp </> "bundler-hs-salvage-test"
      fixture = fixturesRoot </> "format-fail"
      cfg format =
        Config
          { cfgInput = fixture </> "Main.hs",
            cfgLibDirs = [],
            cfgDefines = [],
            cfgGhc = Just pinnedGhc,
            cfgRenameCmd = Nothing,
            cfgFormat = format,
            cfgMinify = noMinify,
            cfgTreeShake = noTreeShake,
            cfgEmbedPosition = EmbedAfter
          }
  createDirectoryIfMissing True sandbox
  mapM_ (removeFile . (sandbox </>)) =<< listDirectory sandbox
  -- Steer the salvage file into a sandbox we can inspect.
  setEnv "TMPDIR" sandbox
  raw <- bundle (cfg FormatNone)
  result <- bundle (cfg (FormatCmd "false"))
  setEnv "TMPDIR" systemTmp
  expected <- either (assertFailure . renderBundleError) pure raw
  case result of
    Right _ -> assertFailure "expected the failing formatter to abort bundling"
    Left (FormatCmdError {}) -> pure ()
    Left err -> assertFailure ("unexpected error: " <> renderBundleError err)
  saved <- filter (isPrefixOf "bundler-hs") <$> listDirectory sandbox
  case saved of
    [file] -> do
      contents <- readFile (sandbox </> file)
      assertEqual "salvaged bundle matches the unformatted output" expected contents
    _ ->
      assertBool ("expected exactly one salvaged bundle, found: " <> show saved) False
  removeDirectoryRecursive sandbox

-- | A plain bundling of one fixture under the given compiler setting.
plainConfig :: FilePath -> Maybe FilePath -> Config
plainConfig fixture ghc =
  Config
    { cfgInput = fixture </> "Main.hs",
      cfgLibDirs = [fixture </> "lib"],
      cfgDefines = [],
      cfgGhc = ghc,
      cfgRenameCmd = Nothing,
      cfgFormat = FormatNone,
      cfgMinify = noMinify,
      cfgTreeShake = noTreeShake,
      cfgEmbedPosition = EmbedAfter
    }

-- | The built-in Prelude names are what a compiler of that era reports, so
-- bundling without a ghc gives the same names as bundling with one.
builtinPreludeMatchesGhc :: TestTree
builtinPreludeMatchesGhc =
  testCase "built-in Prelude names match a real interface" $ do
    dump <-
      readFile
        ("test" </> "fake-ghc" </> "9.10.3" </> "lib" </> "x86_64-linux-ghc-9.10.3" </> "base-4.20.2.0" </> "Prelude" <.> "hi")
    assertEqual "names" (tgPrelude builtinGhc) (parsePreludeExports dump)

ghcVersionMacro :: TestTree
ghcVersionMacro =
  testCase "__GLASGOW_HASKELL__ follows the compiler version" $ do
    assertEqual "9.8.4" 908 (glasgowHaskell (TargetGhc [9, 8, 4] mempty))
    assertEqual "9.10.3" 910 (glasgowHaskell (TargetGhc [9, 10, 3] mempty))

-- | Without --ghc a machine that has no ghc still bundles, on the built-in
-- assumptions: foldl' counts as a Prelude name and the library's own gets
-- its suffix.
missingGhcFallsBack :: TestTree
missingGhcFallsBack =
  testCase "no ghc on PATH falls back to the built-in Prelude" $ do
    path <- getEnv "PATH"
    result <-
      bracket_
        (setEnv "PATH" "")
        (setEnv "PATH" path)
        (bundle (plainConfig (fixturesRoot </> "prelude-ghc-version") Nothing))
    out <- either (assertFailure . renderBundleError) pure result
    assertBool ("foldl' was not moved out of Prelude's way:\n" <> out) ("foldl'F ::" `isInfixOf` out)

-- | A compiler the user named must answer: silently assuming another one
-- would defeat the flag.
unusableExplicitGhcFails :: TestTree
unusableExplicitGhcFails =
  testCase "--ghc naming an unusable compiler is an error" $ do
    let fixture = fixturesRoot </> "prelude-ghc-version"
    result <- bundle (plainConfig fixture (Just (fixture </> "no-such-ghc")))
    case result of
      Left (GhcProbeError {}) -> pure ()
      Left err -> assertFailure ("unexpected error: " <> renderBundleError err)
      Right _ -> assertFailure "expected bundling to fail"

-- | Haskell source is UTF-8 whatever the locale says, so the bundler must
-- not read it through the locale encoding: under @LC_ALL=C@ that either
-- mangles every non-ASCII character or dies with a decoding error.
utf8UnderNonUtf8Locale :: TestTree
utf8UnderNonUtf8Locale =
  testCase "non-ASCII source survives a non-UTF-8 locale" $ do
    let fixture = fixturesRoot </> "utf8-format-cmd"
        cfg = plainConfig fixture (Just pinnedGhc)
    locale <- getLocaleEncoding
    result <-
      bracket_ (setLocaleEncoding char8) (setLocaleEncoding locale) (bundle cfg)
    out <- either (assertFailure . renderBundleError) pure result
    assertBool
      ("the user file's comment was mangled:\n" <> out)
      ("-- 日本語のコメント" `isInfixOf` out)
    assertBool
      ("the library's string literal was mangled:\n" <> out)
      ("\"こんにちは\"" `isInfixOf` out)

-- | One golden test per fixture directory. The @args@ file holds
-- whitespace-separated CLI arguments; @{DIR}@ tokens and relative
-- input/src paths are rebased onto the fixture directory. A fixture with
-- @expected.err.golden@ asserts the rendered bundling error; otherwise
-- @expected.golden@ asserts stdout (and, with @HSB_TEST_COMPILE=1@, that
-- the bundle compiles under @ghc -fno-code@). Unless the arguments name a
-- compiler with @--ghc@, the fixture is bundled for 'pinnedGhc'.
-- A @known-broken@ file exempts the fixture from the compile check and
-- says why: its golden records output that does /not/ compile, pinning a
-- limitation of the bundler so that fixing it shows up as a golden diff.
fixtureTest :: Bool -> FilePath -> IO TestTree
fixtureTest compileGate name = do
  let dir = fixturesRoot </> name
  errCase <- doesFileExist (dir </> "expected.err.golden")
  knownBroken <- doesFileExist (dir </> "known-broken")
  let golden = dir </> if errCase then "expected.err.golden" else "expected.golden"
  pure . goldenVsString name golden $ do
    args <- map (substDir dir) . words <$> readFile (dir </> "args")
    cfg <- either fail pure (parseConfigFromArgs args)
    let rebased =
          cfg
            { cfgInput = dir </> cfgInput cfg,
              cfgLibDirs = map (dir </>) (cfgLibDirs cfg),
              cfgGhc = Just (fromMaybe pinnedGhc (cfgGhc cfg))
            }
    result <- bundle rebased
    case (errCase, result) of
      (False, Right out) -> do
        when (compileGate && not knownBroken) (assertCompiles name out)
        pure (utf8Bytes out)
      (False, Left err) -> fail ("unexpected bundling error:\n" <> renderBundleError err)
      (True, Left err) -> pure (utf8Bytes (renderBundleError err))
      (True, Right _) -> fail "expected a bundling error, but bundling succeeded"

-- | Golden files are UTF-8, like the Haskell sources they hold. Packing a
-- 'String' byte-per-'Char' would truncate every non-ASCII character.
utf8Bytes :: String -> LBS.ByteString
utf8Bytes = LBS.fromStrict . TE.encodeUtf8 . T.pack

substDir :: FilePath -> String -> String
substDir dir s = case s of
  [] -> []
  _
    | "{DIR}" `isPrefixOf` s -> dir <> substDir dir (drop 5 s)
    | (c : cs) <- s -> c : substDir dir cs

assertCompiles :: String -> String -> IO ()
assertCompiles name out = do
  tmp <- getTemporaryDirectory
  (path, h) <- openTempFile tmp (name <> ".hs")
  hSetEncoding h utf8
  hPutStr h out
  hClose h
  (code, _, ghcErr) <- readProcessWithExitCode "ghc" ["-fno-code", path] ""
  removeFile path
  case code of
    ExitSuccess -> pure ()
    ExitFailure _ -> fail ("bundle does not compile:\n" <> ghcErr)
