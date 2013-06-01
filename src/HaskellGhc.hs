{-# LANGUAGE CPP #-}

module HaskellGhc where

-- container
import Data.Tree (Tree(Node,rootLabel))

-- local imports
import Language.Astview.Language hiding (parse)
import qualified Language.Astview.SourceLocation as SrcLoc

import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Annotated.Syntax
import Language.Haskell.Exts.SrcLoc

-- GHC as a library
import qualified DynFlags      as GHC
import qualified GHC           as GHC
import qualified GHC.Paths     as GHC
import qualified NameSet       as GHC
-- import qualified Outputable    as GHC
import System.IO.Unsafe (unsafePerformIO)

-- import Language.Astview.DataTree (data2tree)
-- syb
import Data.Generics (Data
                     ,extQ
                     ,gmapQ
                     ,showConstr
                     ,toConstr)
import Data.Typeable
import qualified GHC.SYB.Utils as SYB

-- ---------------------------------------------------------------------

haskellghc :: Language
haskellghc = Language
  "Haskell"
  "Haskell"
  [".hs",".lhs"]
  parHaskellGhc
  (data2treeGhc::(GHC.Located (GHC.HsModule GHC.RdrName)) ->Tree String)
  toSrcLoc

parHaskell :: String -> Either Error (Module SrcSpan)
parHaskell s =
  case parse s of
    ParseOk t   -> Right t
    ParseFailed (SrcLoc _ l c) m ->
      Left $ ErrLocation (SrcLoc.SrcPosition l c) m

{-
GHC pure parser
parserSource :: String	-- Haskell module source text (full Unicode is supported)
 -> DynFlags	-- the flags
 -> FilePath	-- the filename (for source locations)
 -> Either ErrorMessages (WarningMessages, Located (HsModule RdrName))
-}
parHaskellGhc :: String -> Either Error (GHC.Located (GHC.HsModule GHC.RdrName)) 
parHaskellGhc s =
  let
    df = unsafePerformIO getDynFlags
    res = GHC.parser s df "filename"
  in
    case res of
      Left err    -> Left(ErrLocation (SrcLoc.SrcPosition 1 1) "")
      Right (_,r) -> Right r

getDynFlags :: IO GHC.DynFlags
getDynFlags = do
#if __GLASGOW_HASKELL__ > 704
  GHC.defaultErrorHandler GHC.defaultFatalMessager GHC.defaultFlushOut $ do
#else
  GHC.defaultErrorHandler GHC.defaultLogAction $ do
#endif
    GHC.runGhc (Just GHC.libdir) $ do
      dflags <- GHC.getSessionDynFlags
      return dflags



toSrcLoc :: Tree String -> Maybe SrcLoc.SrcLocation
toSrcLoc (Node "SrcSpan" [_,c1,c2,c3,c4]) =
  Just $ SrcLoc.SrcSpan (to c1) (to c2) (to c3) (to c4)
   where
     to :: Tree String -> Int
     to = read . rootLabel
toSrcLoc _        = Nothing


-- ---------------------------------------------------------------------

-- |Trealise Data to Tree (from SYB 2, sec. 3.4 )
data2tree :: Data a => a -> Tree String
data2tree = gdefault `extQ` atString
  where
    atString x = Node x []
    gdefault x = Node (showConstr $ toConstr x) (gmapQ data2tree x)

data2treeGhc :: Data a => a -> Tree String
-- data2treeGhc v = Node "vvv" []
{- -}
data2treeGhc v
  | checkItemParser v = Node "vvv" []
  | otherwise = (gdefault `extQ` atString) v
  where
    atString x = Node x []
    gdefault x = Node (showConstr $ toConstr x) (gmapQ data2treeGhc x)
{- -}

-- | Checks whether the current item is undesirable for analysis in the current
-- AST Stage.
checkItemStage :: Typeable a => SYB.Stage -> a -> Bool
checkItemStage stage x = (const False `extQ` postTcType `extQ` fixity `extQ` nameSet) x
  where nameSet    = const (stage `elem` [SYB.Parser,SYB.TypeChecker]) :: GHC.NameSet    -> Bool
        postTcType = const (stage < SYB.TypeChecker                  ) :: GHC.PostTcType -> Bool
        fixity     = const (stage < SYB.Renamer                      ) :: GHC.Fixity     -> Bool

checkItemRenamer :: Typeable a => a -> Bool
checkItemRenamer x = checkItemStage SYB.Renamer x

checkItemParser :: Typeable a => a -> Bool
checkItemParser x = checkItemStage SYB.Parser x

{-
everythingStaged :: SYB.Stage -> (r -> r -> r) -> r -> SYB.GenericQ r -> SYB.GenericQ r
everythingStaged stage k z f x
  | checkItemStage stage x = z
  | otherwise = foldl k (f x) (gmapQ (everythingStaged stage k z f) x)
-}
