{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}

module Data.Struere.Editor
    ( main
    ) where

import           Control.Monad
import qualified Data.ByteString.Lazy             as BS
import           Data.Char
import           Data.Dynamic
import           Data.Maybe
import           Data.Reify
import qualified Data.Text                        as T
import           Debug.Trace
import           Prelude                          hiding (pure, (<$>), (<*>))
import qualified Web.KeyCode                      as Key

import qualified Graphics.UI.Threepenny           as UI
import           Graphics.UI.Threepenny.Core      hiding (value)
import           Graphics.UI.Threepenny.Events


import           Data.Struere.Editor.Brick
import           Data.Struere.Editor.Carpenter
import           Data.Struere.Editor.Editable
import           Data.Struere.Editor.Renderer
import           Data.Struere.Editor.Struere
import qualified Data.Struere.Position            as Pos
import           Data.Struere.Struct
import           Data.Struere.Syntax
import           Data.Struere.Util

import           Data.Struere.Editor.Mode.Haskell

main :: IO ()
main = do

    -- r <- reify (test :: Archivist Radiographer Test)
    -- putStrLn $ unlines $ map show $ fst r
    -- putStrLn $ snd r
    -- print =<< reify' test
    startGUI defaultConfig setup


setup :: Window -> UI ()
setup window = do
    return window # set title "Struere"

    body <- getBody window

    let key = keypress body


    button <- UI.new

    str <- accumB "" $ (:) <$> key

    element button # sink UI.text str


    widget body #+ [element button, bufferUI window]

    return ()

bufferUI :: Window -> UI Element
bufferUI window = do
    buf <- UI.new

    ic <- keypress <$> getBody window
    kc <- keydown  <$> getBody window

    let Just up = builder test test1
        Just st = xray test test1
        cs      = fromPosition st (Pos.fromList [0, 1]) InsertCaret
        initc   = Context cs st test1 up test
        Just iui = render test (carets initc) test1

        input   = unionWith const (InputChar <$> ic) (KeyCode <$> kc)


    -- editorB <- accumB (Editor $ Buffer "test" up) never
    --     :: UI (Behavior Editor)

    -- rec cb <- do
    --         let is = instrs (fst <$> cb) key
    --             update i (context, b) =
    --                 let (ma, d, cp') = updater (updater context) i
    --                 in  ( context { updater = cp' }
    --                     , maybe Empty id $ runViewer test <$> ma )
    --         accumB (initc, bu) $ update <$> is
    --             :: UI (Behavior (Context, Brick))

    -- brick <- brick . snd <$> cb

    -- initb <- brick (carets initc) initui
    el <- iui
    return buf # set UI.children [el]

    traceShow (carets initc, struct initc) $ return ()

    mainB <- mainAccum initc input

    onChanges mainB $ \(c, ui) -> do
        -- b <- brick (carets c) a
        el <- ui
        return buf # set UI.children [el]

    -- return buf # sink UI.children (_ (:[]) . brick . snd <$> cb)

    return buf

mainAccum :: Context -> Event Input -> UI (Behavior (Context, UI Element))
mainAccum initc charE =
    accumB (initc, UI.new) $ (`fmap` charE) $ \input (context, el) ->
        case input of
            KeyCode c -> case Key.keyCodeLookup c of
                Key.ArrowUp    -> moveCarets CaretUp   context
                Key.ArrowDown  -> moveCarets CaretDown context
                Key.ArrowRight -> moveCarets CaretNext context
                Key.ArrowLeft  -> moveCarets CaretPrev context
                Key.Delete     -> instrs IDelete context
                _              -> (context, el)
            InputChar c -> instrs (ISet $ toDyn c) context

instrs :: Instr -> Context -> (Context, UI Element)
instrs i context =
    let is       = traceShowId $ instrOn i <$> carets context
        (ma, up) = update (updater context) is
        el       = fromMaybe UI.new $ do
            a <- ma
            render (syntax context) (carets context) a
        mst      = ma >>= xray (syntax context)
    in  (context { struct  = fromMaybe (struct context) mst
                 , value   = fromMaybe (value  context) ma
                 , updater = up } , el)

instrOn :: Instr -> Caret -> Instr
instrOn i NoCaret = mempty
instrOn i _       = i


moveCarets :: CaretMove -> Context -> (Context, UI Element)
moveCarets mv context =
    let cs  = carets context
        mcs = move mv (struct context) cs
        -- rcs = railTop (struct context) mcs
    in trace (unlines $ show (struct context) :  map show [cs, mcs])
        ( context { carets = mcs }
        , fromMaybe UI.new $ render (syntax context) mcs (value context)
        )

-- instrs :: Behavior Context -> Event Char -> Event Instrs
-- instrs contextB charE =
--     (\context c -> case Key.keyCodeLookup $ ord c of
--             Key.Enter -> mempty
--             _         -> const (ISet $ toDyn c) <$> carets context)
--     <$> contextB <@> charE

-- brick :: Pos.Carets -> Brick -> UI Element
-- brick c b = do
--     el <- case b of
--     -- Plane x -> mapM (\c -> UI.span # set UI.text [c]) x
--         Token c  -> UI.span # set UI.text [c]
--         Array xs -> do
--             bs <- zipWithM brick (Pos.listSubInf c) xs
--             UI.new # set UI.children bs
--         Empty    -> UI.span # set UI.text "<empty>"
--     unless (null $ Pos.roots c) $ void $
--         return el
--         # set UI.style [ ("background", "black")
--                     , ("color",      "white")]
--     return el


data KeyModifier = KeyModifier
    { altKey   :: Bool
    , ctrlKey  :: Bool
    , shiftKey :: Bool
    , metaKey  :: Bool
    } deriving (Eq, Show)


-- hoge :: Behavior

    -- print $ encode (Instr 2 False)
    -- let Right hask = parser "a = a" :: Either String HaskellFile
    --     r          = runCarpenter structure
    --                  [Root $ ISet $ toDyn hask] :: Result HaskellFile
        -- initc = Context scaf

    -- let
    --     hoge = name1
    --     str = name
    --     -- str = sub defaultLevel
    --     --     $ (cons <$> letter <*> pure [])
    --     --     <|> (nil <$> pure ())
    --     Just (b, c0) = builder str hoge
    --     (a1, d1, c1) = updater c0 [] -- [Root $ ISet $ toDyn ("bcd" :: String)]
    --     (a2, d2, c2) = updater c1
    --                    -- [Root $ ISet $ toDyn '1']
    --                    -- [Root $ ISet $ toDyn ("xy" :: String)]
    --                    -- []
    --                    [ Sub 0 [Sub 0 [Root $ ISet $ toDyn 'a']]
    --                    , Sub 0 [Sub 2 [Root $ ISet $ toDyn 'v']]]
    --                    -- [Sub 0 []]
    --     (a3, d3, c3) = updater c2 [Root $ ISet $ toDyn ("foo" :: String)]

    -- print hoge
    -- print b
    -- putStrLn ""
    -- print a1
    -- print d1
    -- putStrLn ""
    -- print a2
    -- print d2
    -- putStrLn ""
    -- print a3
    -- print d3
    -- print "end"

    -- WS.runServer "0.0.0.0" 9160 application

-- application :: WS.ServerApp
-- application pending = do

--     -- let Right hask = parser "a = a"
--     --     Just scaf  = runBuilder structure hask
--     --     initc = Context scaf

--     -- print hask

--     conn <- WS.acceptRequest pending
--     WS.forkPingThread conn 30

--     -- let c = Context $ updater

--     let Just (b, c) = builder test test1

--     let msg = LoadBuffer 0 $ Buffer "test" b
--     WS.sendTextData conn $ encode msg

--     loop conn $ Context { updater = c }


-- loop :: WS.Connection -> Context -> IO ()
-- loop conn c = do
--     msg <- WS.receiveData conn :: IO BS.ByteString

--     let mevent = decode msg :: Maybe Event
--         is = case mevent of
--             Just (KeyPress km x) ->
--                 let p = fromEnum x
--                 in  [Sub 0 [Root $ ISet $ toDyn p]]
--             _                    -> []
--         (ma, d, c') = updater (updater c) is

--     -- let r = runCarpenter (updater c) Nothing []

--     print (decode msg :: Maybe Event)
--     loop conn c
