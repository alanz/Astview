module Language.Astview.Registry where

-- hint
import Language.Haskell.Interpreter hiding ((:=),set)

-- glob
import System.FilePath.Glob (compile,globDir)

-- astview-utils
import Language.Astview.Language (Language)

-- local
import Paths_astview (getDataDir) -- by cabal

-- | loads the language registration and all modules in data dir
loadLanguages :: IO [Language]
loadLanguages = do
  -- find additional modules in data
  (glob,_) <- globDir [compile "data/**/*.hs"] =<< getDataDir
  let modules = head glob
  -- run Interpreter
  langs' <- runInterpreter $ interpretLangs modules
  case langs' of
    Right l -> return l 
    Left err -> error (show err)

-- | interprets the modules and returns all languages found.
interpretLangs :: [FilePath] -> Interpreter [Language]
interpretLangs modules = do 
  loadModules modules
  setTopLevelModules ["Languages"]
  return =<< interpret "languages" (as :: [Language])

