#!/usr/bin/runhaskell
{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
import Shelly

import qualified Data.Text.Lazy as LT
import Data.Text.Lazy (Text)
import System.Console.CmdArgs

-- MAIN

main :: IO ()
main = shelly $ print_stdout False $ print_commands True $ do
  opts' <- liftIO $ cmdArgs $ CmdOptions
      { dev      = Nothing &= help "Development mode (false)"
      , compress = False   &= help "Compressed output (disabled by --dev)"
      , pretty   = False   &= help "Pretty output (enabled by --dev)"
      , outdir   = "build" &= help "Directory where Javascript files will be generated" &= typ "PATH"
      , indir    = "src"   &= help "Directory where Haskell source files can be found"  &= typ "PATH"
      , autorun  = False   &= help "Automatically execute Main.main if found"
      , modules  = def     &= args                                                      &= typ "MODULES"
      } &= summary "Build script for fay projects" &= program "Build.hs"
  let opts = resolveDev opts'
  let pret = pretty opts
  let idir = LT.pack $ indir opts
  let odir = LT.pack $ outdir opts
  let mods = map LT.pack $ modules opts
  -- Warnings
  when (null mods) $ errorExit "No included modules.\nUsage: For basic information, try the `--help' option."
  -- Runtime
  rts <- generateRTS odir
  -- Generate Wrapper Module
  wrapper <- generateWrapper idir mods
  -- Generate everything
  stdlib <- generateStdlib pret idir odir wrapper
  dispatcher <- generateDispatcher pret idir odir wrapper
  -- Compile modules
  moduli <- mapM (generateModule pret idir odir) mods
  -- Concatenate
  let build = LT.unlines $ [rts, stdlib, dispatcher] ++ moduli ++ if autorun opts then ["if(Main) { var __main__ = new Main(); if(__main__.Main$main) { __main__._(__main__.Main$main);}}"] else []
  writefile (fromText $ outName odir "build") build
  -- Compress
  when (compress opts) $ compressOut odir "build"
  echo "All Done"

-- EVERYTHING ELSE FOLLOWS

data CmdOptions = CmdOptions
  { dev      :: Maybe Bool
  , compress :: Bool
  , pretty   :: Bool
  , outdir   :: String
  , indir    :: String
  , autorun  :: Bool
  , modules  :: [String]
  }
  deriving (Show,Data,Typeable)

withDev :: Bool -> CmdOptions -> CmdOptions
withDev d c = c{dev = Just d, compress = not d, pretty = d}

withProd :: Bool -> CmdOptions -> CmdOptions
withProd = withDev . not

resolveDev :: CmdOptions -> CmdOptions
resolveDev c = case dev c of
    Nothing -> c
    Just d -> withDev d c

generateRTS :: Text -> Sh Text
generateRTS odir = do
  rts <- run "fay" ["--print-runtime"]
  writefile (fromText $ outName odir "rts") rts
  return rts

commonFayOpts :: Text -> [Text]
-- commonFayOpts = ["--no-ghc", "--no-rts", "--include", "src"]
commonFayOpts idir = ["--no-rts", "--include", idir]

makeOpts :: Bool -> Text -> Text -> Text -> Text -> [Text] -> [Text]
makeOpts p idir odir inf outf rest = (if p then ["--pretty"] else []) ++ [inName idir inf, "-o", outf] ++ rest

inName :: Text -> Text -> Text
inName dir n = LT.concat [dir, "/", n, ".hs"]

outName :: Text -> Text -> Text
outName dir n = LT.concat [dir, "/", n, ".js"]

stdlibFayOpts :: [Text]
stdlibFayOpts = ["--stdlib", "--no-dispatcher", "--naked"]

dispatcherFayOpts :: [Text]
dispatcherFayOpts = ["--no-stdlib", "--dispatcher", "--naked"]

moduleFayOpts :: [Text]
moduleFayOpts = ["--no-stdlib", "--no-dispatcher", "--library", "--no-builtins"]

fayCmd :: Text -> [Text] -> Sh ()
fayCmd idir = command_ "fay" $ commonFayOpts idir

runFay :: Bool -> Text -> Text -> Text -> Text -> [Text] -> Sh Text
runFay p idir odir w n rest = do
  fayCmd idir $ makeOpts p idir odir w nm rest
  readfile $ fromText nm
  where
    nm = outName odir n

generateDispatcher :: Bool -> Text -> Text -> Text -> Sh Text
generateDispatcher p idir odir w = runFay p idir odir w "dispatcher" dispatcherFayOpts

generateStdlib :: Bool -> Text -> Text -> Text -> Sh Text
generateStdlib p idir odir w = runFay p idir odir w "stdlib" stdlibFayOpts

generateModule :: Bool -> Text -> Text -> Text -> Sh Text
generateModule p idir odir n = runFay p idir odir n n moduleFayOpts

generateWrapper :: Text -> [Text] -> Sh Text
generateWrapper dir mods = do
  writefile (fromText $ inName dir nm) $ LT.unlines contents
  return nm
  where
    nm = "__Wrapper__"
    contents =  "module Wrapper where" : map (LT.append "import ") ("FFI" : "Prelude" : mods)

compressOut :: Text -> Text -> Sh ()
compressOut odir file = run_ "java" ["-jar", "tools/compiler.jar", "--js", outName odir file, "--js_output_file", outName odir $ LT.append file "_compressed"]

