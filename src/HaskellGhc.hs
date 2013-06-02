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
import qualified Bag           as GHC
import qualified DynFlags      as GHC
import qualified GHC           as GHC
import qualified GHC.Paths     as GHC
import qualified NameSet       as GHC
import qualified OccName       as GHC
import qualified RdrName       as GHC
import qualified Outputable    as GHC
import qualified Var           as GHC
import System.IO.Unsafe (unsafePerformIO)

-- import Language.Astview.DataTree (data2tree)
-- syb
import Data.Generics (Data
                     ,ext1Q
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
  -- (data2treeGhc::(GHC.Located (GHC.HsModule GHC.RdrName)) ->Tree String)
  (data2treeS::(GHC.Located (GHC.HsModule GHC.RdrName)) ->Tree String)
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


mppr :: (GHC.Outputable a) => a -> String
mppr = prettyprint

prettyprint :: (GHC.Outputable a) => a -> String
prettyprint = prettyprintdf df
  where
    df = unsafePerformIO getDynFlags

prettyprintdf :: (GHC.Outputable a) => GHC.DynFlags -> a -> String
#if __GLASGOW_HASKELL__ > 704
prettyprintdf df x = GHC.renderWithStyle df (GHC.ppr x) (GHC.mkUserStyle GHC.neverQualify GHC.AllTheWay)
#else
prettyprintdf df x = GHC.renderWithStyle    (GHC.ppr x) (GHC.mkUserStyle GHC.neverQualify GHC.AllTheWay)
#endif



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
data2treeGhc v
  | checkItemParser v = Node "*not populated*" []
  | otherwise = (gdefault `extQ` atString) v
  where
    atString x = Node x []
    gdefault x = Node (showConstr $ toConstr x) (gmapQ data2treeGhc x)

-- ---------------------------------------------------------------------
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

-- ---------------------------------------------------------------------

data2treeGhc' :: Data a => a -> Tree String
data2treeGhc' v
  | checkItemParser v = Node "*not populated*" []
  | otherwise = (gdefault `extQ` atString) v
  where
    atString x = Node x []
    gdefault x = Node (showConstr $ toConstr x) (gmapQ data2treeGhc' x)



--  extQ :: (Typeable a, Typeable b) => (a -> q) -> (                      b -> q) -> a -> q
-- ext1Q :: (Data a,    Typeable1 t) => (a -> q) -> (forall e. Data e => t e -> q) -> a -> q

data2treeS :: (Data a) => a -> Tree String
data2treeS = data2treeStaged SYB.Parser

-- data2treeStaged :: (Data a,Typeable a) => SYB.Stage -> a -> Tree String
data2treeStaged :: (Data a) => SYB.Stage -> a -> Tree String
data2treeStaged stage =
  generic `ext1Q` list `extQ` ghcname `extQ` occName `extQ` srcSpan
           `extQ` moduleName
           `extQ` bagRdrName `extQ` bagName `extQ` bagVar
           `extQ` overLit
           `extQ` nameSet `extQ` postTcType `extQ` fixity
  where generic :: Data a => a -> Tree String
        generic t = Node (showConstr (toConstr t)) (gmapQ (data2treeStaged stage) t)

        list l    = (Node ("List:") (map (data2treeStaged stage) l))

        ghcname :: GHC.Name -> Tree String
        ghcname x = (Node ("{Name: " ++ (mppr x) ++ "}") [])

        occName :: GHC.OccName -> Tree String
        occName x   = (Node ("{OccName: " ++ ((GHC.occNameString x)::String) ++"}") []) 

        -- srcSpan :: GHC.SrcSpan -> Tree String
        -- srcSpan x = (Node ("{"++ (mppr x) ++"}") [])
        srcSpan :: GHC.SrcSpan -> Tree String
        srcSpan x = (Node "SrcSpan" (srcSpanToSubTrees x))

        moduleName :: GHC.ModuleName -> Tree String
        moduleName x = (Node ("{ModuleName: " ++ (mppr x) ++ "}") [])

        bagRdrName:: GHC.Bag (GHC.Located (GHC.HsBind GHC.RdrName)) -> Tree String
        bagRdrName x = Node ("Bag(Located (HsBind RdrName)): ")  [list $ GHC.bagToList x]

        bagName:: GHC.Bag (GHC.Located (GHC.HsBind GHC.Name)) -> Tree String
        bagName x = Node ("Bag(Located (HsBind Name)): ")  [list $ GHC.bagToList x]

        bagVar    :: GHC.Bag (GHC.Located (GHC.HsBind GHC.Var)) -> Tree String
        bagVar x = Node ("Bag(Located (HsBind Var)): ")  [list $ GHC.bagToList x]

        overLit | stage <SYB.TypeChecker = const (Node "{!noRebinadbleInfo placeholder here?!}" []) :: GHC.HsOverLit GHC.RdrName -> Tree String
                | otherwise = \x -> ((Node (SYB.showSDoc_ $ GHC.ppr x) []))

        nameSet | stage `elem` [SYB.Parser,SYB.TypeChecker]
                = const ( Node "{!NameSet placeholder here!}" []) :: GHC.NameSet -> Tree String
                | otherwise
                -- = ("{NameSet: "++) . (++"}") . list . nameSetToList
                = const (Node (("{NameSet: }") ) [])

        postTcType | stage<SYB.TypeChecker = const (Node "{!type placeholder here?!}" []) :: GHC.PostTcType -> Tree String
                   | otherwise     = \x -> ((Node (SYB.showSDoc_ $ GHC.ppr x) [])) 

        fixity | stage<SYB.Renamer = const (Node "{!fixity placeholder here?!}" []) :: GHC.Fixity -> Tree String
               -- | otherwise     = ("{Fixity: "++) . (++"}") . show
               -- | otherwise     = const (Node ( ("{Fixity: "++) . (++"}") . show) [])
               | otherwise     = \x -> (Node ( "{Fixity: "++ (mppr x) ++"}") [])

-- |Create parsable sub-trees to enable linking the source code to the tree
srcSpanToSubTrees :: GHC.SrcSpan -> [Tree String]
srcSpanToSubTrees (GHC.RealSrcSpan ss) =
  [ Node (show $ GHC.srcSpanStartLine ss) []
  , Node (show $ GHC.srcSpanStartCol  ss) []
  , Node (show $ GHC.srcSpanEndLine   ss) []
  , Node (show $ GHC.srcSpanEndCol    ss) []
  ]
srcSpanToSubTrees _ = replicate 4 (Node "-1" [])

-- ---------------------------------------------------------------------
-- From
-- http://hackage.haskell.org/packages/archive/ghc-syb-utils/0.2.1.1/doc/html/src/GHC-SYB-Utils.html#showData
{-
-- | Generic Data-based show, with special cases for GHC Ast types,
--   and simplistic indentation-based layout (the 'Int' parameter);
--   showing abstract types abstractly and avoiding known potholes
--   (based on the 'Stage' that generated the Ast)
showData :: Data a => Stage -> Int -> a -> String
showData stage n =
  generic `ext1Q` list `extQ` string `extQ` fastString `extQ` srcSpan
          `extQ` name `extQ` occName `extQ` moduleName `extQ` var `extQ` dataCon
          `extQ` bagName `extQ` bagRdrName `extQ` bagVar `extQ` nameSet
          `extQ` postTcType `extQ` fixity
  where generic :: Data a => a -> String
        generic t = indent n ++ "(" ++ showConstr (toConstr t)
                 ++ space (concat (intersperse " " (gmapQ (showData stage (n+1)) t))) ++ ")"
        space "" = ""
        space s  = ' ':s
        indent n = "\n" ++ replicate n ' '
        string     = show :: String -> String
        fastString = ("{FastString: "++) . (++"}") . show :: FastString -> String
        list l     = indent n ++ "["
                              ++ concat (intersperse "," (map (showData stage (n+1)) l)) ++ "]"

        name       = ("{Name: "++) . (++"}") . showSDoc_ . ppr :: Name -> String
        occName    = ("{OccName: "++) . (++"}") .  OccName.occNameString
        moduleName = ("{ModuleName: "++) . (++"}") . showSDoc_ . ppr :: ModuleName -> String
        srcSpan    = ("{"++) . (++"}") . showSDoc_ . ppr :: SrcSpan -> String
        var        = ("{Var: "++) . (++"}") . showSDoc_ . ppr :: Var -> String
        dataCon    = ("{DataCon: "++) . (++"}") . showSDoc_ . ppr :: DataCon -> String

        bagRdrName:: Bag (Located (HsBind RdrName)) -> String
        bagRdrName = ("{Bag(Located (HsBind RdrName)): "++) . (++"}") . list . bagToList 
        bagName   :: Bag (Located (HsBind Name)) -> String
        bagName    = ("{Bag(Located (HsBind Name)): "++) . (++"}") . list . bagToList 
        bagVar    :: Bag (Located (HsBind Var)) -> String
        bagVar     = ("{Bag(Located (HsBind Var)): "++) . (++"}") . list . bagToList 

        nameSet | stage `elem` [Parser,TypeChecker]
                = const ("{!NameSet placeholder here!}") :: NameSet -> String
                | otherwise
                = ("{NameSet: "++) . (++"}") . list . nameSetToList

        postTcType | stage<TypeChecker = const "{!type placeholder here?!}" :: PostTcType -> String
                   | otherwise     = showSDoc_ . ppr :: Type -> String

        fixity | stage<Renamer = const "{!fixity placeholder here?!}" :: GHC.Fixity -> String
               | otherwise     = ("{Fixity: "++) . (++"}") . show
-}

