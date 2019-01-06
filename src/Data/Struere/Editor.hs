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
import qualified Data.Text                        as T
import           Debug.Trace
import           Prelude                          hiding (pure, (<$>), (<*>))
import qualified Web.KeyCode                      as Key

import qualified Graphics.UI.Threepenny           as UI
import           Graphics.UI.Threepenny.Core
import           Graphics.UI.Threepenny.Events


import           Data.Struere.Editor.Brick
import           Data.Struere.Editor.Carpenter
import           Data.Struere.Editor.Editable
import qualified Data.Struere.Editor.Position     as Pos
import           Data.Struere.Editor.Struere
import           Data.Struere.Editor.Util
import           Data.Struere.Editor.Viewer
import           Data.Struere.Structural

import           Data.Struere.Editor.Mode.Haskell

main :: IO ()
main = do
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

    let Just (bu, cp) = builder test test1
        initc = Context (Pos.positioned (Pos.fromList [0, 1]) ()) cp

        input = unionWith const (InputChar <$> ic) (KeyCode <$> kc)

    -- editorB <- accumB (Editor $ Buffer "test" cp) never
    --     :: UI (Behavior Editor)

    -- rec cb <- do
    --         let is = instrs (fst <$> cb) key
    --             update i (context, b) =
    --                 let (ma, d, cp') = updater (carpenter context) i
    --                 in  ( context { carpenter = cp' }
    --                     , maybe Empty id $ runViewer test <$> ma )
    --         accumB (initc, bu) $ update <$> is
    --             :: UI (Behavior (Context, Brick))

    -- brick <- brick . snd <$> cb

    initb <- brick (carets initc) bu
    return buf # set UI.children [initb]

    mainB <- mainAccum initc input

    onChanges mainB $ \(c, a) -> do
        b <- brick (carets c) a
        return buf # set UI.children [b]

    -- return buf # sink UI.children (_ (:[]) . brick . snd <$> cb)

    return buf

mainAccum :: Context -> Event Input -> UI (Behavior (Context, Brick))
mainAccum initc charE =
    accumB (initc, Empty) $ (`fmap` charE) $ \input (context, b) ->
    case input of
        KeyCode c -> case Key.keyCodeLookup c of
            Key.ArrowUp    -> (moveCarets (Pos.up 1)   context, b)
            Key.ArrowDown  -> (moveCarets (Pos.down 1) context, b)
            Key.ArrowRight -> (moveCarets (Pos.next 1) context, b)
            Key.ArrowLeft  -> (moveCarets (Pos.prev 1) context, b)
            _              -> (context, b)
        InputChar c ->
            let is = const (ISet $ toDyn c) <$> carets context
                (ma, d, cp) = updater (carpenter context) is
                b'          = maybe Empty id $ runViewer test <$> ma
             in  (context { carpenter = cp } , b')


moveCarets :: Pos.Path -> Context -> Context
moveCarets p context =
    let cs  = carets context
        mcs = Pos.move p cs
        rcs = railTop (carpenter context) mcs
    in trace (unlines $ show cs : show mcs : show rcs : [])
        context { carets = rcs }

instrs :: Behavior Context -> Event Char -> Event Instrs
instrs contextB charE =
    (\context c -> case Key.keyCodeLookup $ ord c of
            Key.Enter -> mempty
            _         -> const (ISet $ toDyn c) <$> carets context)
    <$> contextB <@> charE

brick :: Carets -> Brick -> UI Element
brick c b = do
    el <- case b of
    -- Plane x -> mapM (\c -> UI.span # set UI.text [c]) x
        Plane c  -> UI.span # set UI.text [c]
        Array xs -> do
            bs <- zipWithM brick (Pos.listSubInf c) xs
            UI.new # set UI.children bs
        Empty    -> UI.span # set UI.text "<empty>"
    unless (null $ Pos.roots c) $ void $
        return el
        # set style [ ("background", "black")
                    , ("color",      "white")]
    return el


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

--     -- let c = Context $ carpenter

--     let Just (b, c) = builder test test1

--     let msg = LoadBuffer 0 $ Buffer "test" b
--     WS.sendTextData conn $ encode msg

--     loop conn $ Context { carpenter = c }


-- loop :: WS.Connection -> Context -> IO ()
-- loop conn c = do
--     msg <- WS.receiveData conn :: IO BS.ByteString

--     let mevent = decode msg :: Maybe Event
--         is = case mevent of
--             Just (KeyPress km x) ->
--                 let p = fromEnum x
--                 in  [Sub 0 [Root $ ISet $ toDyn p]]
--             _                    -> []
--         (ma, d, c') = updater (carpenter c) is

--     -- let r = runCarpenter (carpenter c) Nothing []

--     print (decode msg :: Maybe Event)
--     loop conn c
