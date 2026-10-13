{-# LANGUAGE ScopedTypeVariables #-}

-- | The compiler the bundle is built for.
--
-- The bundle is compiled by a GHC the bundler never sees (a judge's), and
-- two things about it shape the output: its version, which CPP conditionals
-- and cabal @impl(ghc)@ conditions branch on, and what its Prelude exports,
-- which no library name may collide with. Both are asked of a @ghc@
-- executable rather than assumed.
module Bundler.Ghc
  ( TargetGhc (..),
    builtinGhc,
    glasgowHaskell,
    parsePreludeExports,
    resolveTargetGhc,
  )
where

import Bundler.Error
import Bundler.PreludeNames (preludeNames)
import Bundler.Symbols (NsKey (..), OccKey, isOperatorString)
import Control.Exception (IOException, try)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE)
import Data.ByteString.Lazy qualified as LBS
import Data.Char (isAlphaNum, isDigit, isSpace, isUpper)
import Data.List (dropWhileEnd, isPrefixOf, sort)
import Data.Maybe (listToMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.Exit (ExitCode (..))
import System.FilePath ((<.>), (</>))
import System.IO (hPutStrLn, stderr)
import System.Process.Typed (proc, readProcess)
import Text.Read (readMaybe)

data TargetGhc = TargetGhc
  { -- | Version components, most significant first (@[9, 8, 4]@).
    tgVersion :: [Int],
    -- | The alphanumeric names its Prelude exports, as in 'preludeNames'.
    tgPrelude :: Set OccKey
  }
  deriving (Show)

-- | What is assumed when no compiler can be asked: the GHC whose grammar
-- ghc-lib-parser implements.
builtinGhc :: TargetGhc
builtinGhc = TargetGhc [9, 12] preludeNames

-- | The value of @__GLASGOW_HASKELL__@ under this compiler.
glasgowHaskell :: TargetGhc -> Int
glasgowHaskell target = case tgVersion target of
  major : minor : _ -> major * 100 + minor
  [major] -> major * 100
  [] -> 0

-- | An explicitly named compiler (@--ghc@) must answer. Without one the
-- @ghc@ on @PATH@ is asked, and bundling falls back to 'builtinGhc' when it
-- cannot be: a machine with no GHC at all can still bundle.
resolveTargetGhc :: Maybe FilePath -> IO (Either BundleError TargetGhc)
resolveTargetGhc explicit = case explicit of
  Just exe -> either (Left . GhcProbeError exe) Right <$> probeGhc exe
  Nothing -> do
    probed <- probeGhc "ghc"
    case probed of
      Right target -> pure (Right target)
      Left why -> do
        hPutStrLn stderr $
          "note: could not ask ghc for its version and Prelude ("
            <> why
            <> "); assuming GHC 9.12. Pass --ghc to name the compiler."
        pure (Right builtinGhc)

probeGhc :: FilePath -> IO (Either String TargetGhc)
probeGhc exe = runExceptT $ do
  versionText <- run ["--numeric-version"]
  version <- case traverse readMaybe (splitOn '.' (trim versionText)) of
    Just parts@(_ : _) -> pure parts
    _ -> throwE ("unexpected --numeric-version output: " <> show versionText)
  libDir <- trim <$> run ["--print-libdir"]
  iface <- ExceptT (findPreludeIface libDir)
  dump <- run ["--show-iface", iface]
  let prelude = parsePreludeExports dump
  if Set.null prelude
    then throwE ("no exports found in " <> iface)
    else pure (TargetGhc version prelude)
  where
    run args = ExceptT $ do
      result <- try (readProcess (proc exe args))
      pure $ case result of
        Left (e :: IOException) -> Left (show e)
        Right (ExitSuccess, out, _) -> Right (decode out)
        Right (ExitFailure n, _, err) ->
          Left (unwords (exe : args) <> " exited with code " <> show n <> ": " <> trim (decode err))
    decode = T.unpack . TE.decodeUtf8Lenient . LBS.toStrict
    trim = dropWhileEnd isSpace . dropWhile isSpace
    splitOn c s = case break (== c) s of
      (chunk, _ : rest) -> chunk : splitOn c rest
      (chunk, []) -> [chunk]

-- | Prelude's interface file of the @base@ installed under a GHC libdir.
-- Package directories sit in the libdir itself or one level down (under a
-- platform directory), and carry a version and sometimes a hash.
findPreludeIface :: FilePath -> IO (Either String FilePath)
findPreludeIface libDir = do
  subDirs <- listDirs libDir
  baseDirs <- concat <$> traverse baseDirsIn (libDir : subDirs)
  found <-
    traverse
      (\file -> (\there -> [file | there]) <$> doesFileExist file)
      [dir </> "Prelude" <.> ext | dir <- baseDirs, ext <- ["hi", "dyn_hi", "p_hi"]]
  pure (maybe (Left ("no Prelude interface of base under " <> libDir)) Right (listToMaybe (concat found)))
  where
    listDirs dir = do
      isDir <- doesDirectoryExist dir
      if isDir then map (dir </>) . sort <$> listDirectory dir else pure []
    baseDirsIn dir = do
      isDir <- doesDirectoryExist dir
      names <- if isDir then sort <$> listDirectory dir else pure []
      pure [dir </> name | name <- names, isBase name]
    -- base-4.19.2.0, not base-compat-0.14.
    isBase name = "base-" `isPrefixOf` name && all isDigit (take 1 (drop 5 name))

-- | The names in the @exports:@ section of @ghc --show-iface@ output.
--
-- An export is a qualified name, followed by its children in braces when it
-- is a type or class: @GHC.Maybe.Maybe{GHC.Maybe.Just GHC.Maybe.Nothing}@.
-- Operators are left out, as in 'preludeNames'.
parsePreludeExports :: String -> Set OccKey
parsePreludeExports dump = Set.fromList (go False (tokens exportsSection))
  where
    exportsSection =
      unlines
        . takeWhile (all isSpace . take 1)
        . drop 1
        . dropWhile (/= "exports:")
        . map (dropWhileEnd isSpace)
        $ lines dump
    tokens = words . concatMap (\c -> if c `elem` "{}|" then [' ', c, ' '] else [c])
    go _ [] = []
    go inside (t : ts) = case t of
      "{" -> go True ts
      "}" -> go False ts
      "|" -> go inside ts
      _ ->
        let name = unqualify t
         in [classify inside name | not (isOperatorString name)] <> go inside ts
    classify inside name
      | not (startsUpper name) = (NsValue, name)
      | inside = (NsData, name)
      | otherwise = (NsTcCls, name)
    startsUpper = any isUpper . take 1
    -- Drop the leading @Conid.@ components, keeping an operator that is
    -- itself made of dots.
    unqualify s = case span isIdentChar s of
      (component, '.' : rest) | startsUpper component, not (null rest) -> unqualify rest
      _ -> s
    isIdentChar c = isAlphaNum c || c `elem` "_'"
