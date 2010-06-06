{- contains the main GUI functions
 -
 -}
module Language.Astview.GUI where

-- guiactions
import Language.Astview.GUIData
import Language.Astview.GUIActions 

-- base
import Control.Monad.Trans (liftIO)
import Data.IORef
-- filepath
import System.FilePath ((</>))

-- gtk
import Graphics.UI.Gtk hiding (Language) 
import Graphics.UI.Gtk.Gdk.EventM

-- glib
import System.Glib.Signals (ConnectId)

-- glade
import Graphics.UI.Gtk.Glade     

-- gtksourceview
import Graphics.UI.Gtk.SourceView

-- astview-utils
import Language.Astview.Language

-- generated on-the-fly by cabal
import Paths_astview (getDataFileName) 

-- | initiates aststate
buildAststate :: Options -> [Language] -> IO (IORef AstState)
buildAststate opt langs = do
  -- GTK init
  initGUI 

  -- load GladeXML
  Just xml <- xmlNew =<< getDataFileName ("data" </> "astview.glade")
 
  -- get or create widgets
  window   <- xmlGetWidget xml castToWindow "mainWindow"
  treeviewL <- xmlGetWidget xml castToTreeView "treeviewLeft"
  treeviewR <- xmlGetWidget xml castToTreeView "treeviewRight"

  tbL <- buildSourceView opt 
    =<< xmlGetWidget xml castToScrolledWindow "swSourceLeft" 

  tbR <- buildSourceView opt 
    =<< xmlGetWidget xml castToScrolledWindow "swSourceRight" 

  tvConfig <- xmlGetWidget xml castToTextView "tvConfig"

  dlgAbout <-xmlGetWidget xml castToAboutDialog "dlgAbout"

  -- setup combobox
  vbox <- xmlGetWidget xml castToVBox "vboxMain"
  cbox <- comboBoxNewText
  containerAdd vbox cbox
  boxSetChildPacking vbox cbox PackNatural 2 PackEnd
  mapM_ (comboBoxAppendText cbox . buildLabel) langs 

  -- build compound datatype
  let gui = GUI window 
                (treeviewL,treeviewR) 
                (tbL,tbR) 
                tvConfig 
                dlgAbout 
                cbox 
      c = if null langs then undefined else head langs
      state = State 
        { cFile = (unsavedDoc,unsavedDoc)
        , textchanged = (False,False)
        , languages = langs
        , cLang = c
        , cArea = L
        , config = Configuration [] [] 
        }

  r <- newIORef $ AstState state gui opt
   
  hooks r

  -- get all menuitems from xml and register guiactions to them
  mapM_ (registerMenuAction xml r) menuActions
  
  return r

-- -------------------------------------------------------------------
-- ** some helper functions
-- -------------------------------------------------------------------

-- |builds combobox label for a language
buildLabel :: Language -> String
buildLabel l = 
  name l
  ++ " ["
  ++ concatMap (" "++) (exts l)
  ++ "]"

-- | setup the GtkSourceView and add it to the ScrollPane. return the 
-- underlying textbuffer
buildSourceView :: Options -> ScrolledWindow -> IO SourceBuffer
buildSourceView opt sw = do
  sourceBuffer <- sourceBufferNew Nothing
  sourceBufferSetHighlightSyntax sourceBuffer True
  sourceView <- sourceViewNewWithBuffer sourceBuffer
  sourceViewSetShowLineNumbers sourceView True
  sourceViewSetHighlightCurrentLine sourceView True
  srcfont <- fontDescriptionFromString $ 
    font opt ++" "++show (fsize opt)
  widgetModifyFont sourceView (Just srcfont)
  containerAdd sw sourceView
  return sourceBuffer

-- | registers one GUIAction with a MenuItem
registerMenuAction 
  :: GladeXML -> IORef AstState 
  -> (String,AstAction ()) -> IO (ConnectId MenuItem)
registerMenuAction xml ref (gtkId,action) = do
  item <- xmlGetWidget xml castToMenuItem gtkId
  onActivateLeaf item $ action ref

-- | adds actions to some widgets
hooks :: AstAction (ConnectId Window)
hooks ref = do
  gui <- getGui ref
  -- textbuffer
  onBufferChanged (fst $ sb gui) $ 
    actionBufferChanged L ref
  onBufferChanged (snd $ sb gui) $ 
    actionBufferChanged R ref  

  -- ctrl+p to reparse
  window gui `on` keyPressEvent $ tryEvent $ do
    [Control] <- eventModifier
    "p" <- eventKeyName
    liftIO $ actionReparseAll ref 

  cbox gui `on` changed $ do
    i <- comboBoxGetActive (cbox gui) 
    langs <- getLangs ref
    let lang = langs!!i
    setLanguage lang ref 
    actionReparseAll ref
    comboBoxSetActive (cbox gui) i
    
  dlgAbout gui `onResponse` (const $ widgetHide $ dlgAbout gui)
        
  window gui `on` deleteEvent $ tryEvent $ liftIO $ actionQuit ref
  
  -- window    
  onDestroy (window gui) mainQuit
