{-# LANGUAGE ExistentialQuantification, FlexibleInstances, GeneralizedNewtypeDeriving, TypeFamilies, TypeSynonymInstances #-}
module Shade.Haste.Internal.Core where
import Control.Applicative (Applicative)
import Control.Concurrent.MVar (MVar, newMVar, readMVar, modifyMVar_)
import Control.Monad (void)
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Writer.Strict (WriterT, runWriterT, tell)
import Data.DList (DList)
import qualified Data.DList as D
import Data.Maybe (catMaybes)
import Haste.DOM (Elem)
import Haste.Prim (JSString, fromJSStr)
import Shade.Core
import qualified Shade.Haste.Internal.React as R

-- a is the return type of the Async. various "i" sources can trigger it,
-- and various handlers can be installed into the callback.
-- the callback is i -> IO () because the actual buttons etc will call it
-- with their triggering data. "listen" ensures it's converted to an 'a'
-- We could have the actual callbacks use the i->a function too, but
-- probably same difference?
data AsyncSource a = forall i . AsyncSource (i -> a, MVar [i -> IO ()])
data AsyncImpl a = AsyncImpl [AsyncSource a]

instance Functor AsyncImpl where
  fmap f (AsyncImpl as) = AsyncImpl $ map f' as
    where f' (AsyncSource (a, b)) = AsyncSource (f . a, b)

instance FireFirst AsyncImpl where
  fireFirst as = AsyncImpl $ concatMap unwrap as
    where
      unwrap (AsyncImpl v) = v

mkAsyncs :: IO ([(R.EventHandler, IO Bool)], ElemAsyncs ShadeHaste)
mkAsyncs =
  do (mvClick, asClick) <- mkMVar
     (mvDoubleClick, asDoubleClick) <- mkMVar
     (mvChange, asChange) <- mkMVar
     (mvKeyUp, asKeyUp) <- mkMVar
     (mvKeyPress, asKeyPress) <- mkMVar
     (mvKeyDown, asKeyDown) <- mkMVar
     (mvBlur, asBlur) <- mkMVar
     return ( [(R.onClick (fire mvClick), someListeners mvClick)
              ,(R.onDoubleClick (fire mvDoubleClick), someListeners mvDoubleClick)
              ,(R.onChange (fire mvChange), someListeners mvChange)
              ,(R.onKeyUp (fire mvKeyUp), someListeners mvKeyUp)
              ,(R.onKeyPress (fire mvKeyPress), someListeners mvKeyPress)
              ,(R.onKeyDown (fire mvKeyDown), someListeners mvKeyDown)
              ,(R.onBlur (fire mvBlur), someListeners mvBlur)
              ]
            , ElemAsyncs { onClick = asClick
                         , onDoubleClick = asDoubleClick
                         , onChange = asChange
                         , onKeyUp = asKeyUp
                         , onKeyPress = asKeyPress
                         , onKeyDown = asKeyDown
                         , onBlur = asBlur})
  where
    mkMVar = do mv <- newMVar []
                return (mv, AsyncImpl [AsyncSource (id, mv)])
    fire mv evt = do cbs <- readMVar mv
                     mapM_ ($! evt) cbs -- React pools and wrecks existing event objects, so we have to strictly read them asap.
    someListeners mv = fmap (not . null) (readMVar mv)


listenedCallbacks = mapM $ \(h,s) -> do
    some <- s
    return $ if some
      then Just h
      else Nothing

defaultElement :: (t -> [R.EventHandler] -> [R.React] -> IO R.React)
               -> t
               -> ShadeHaste a
               -> ShadeHaste (ElemAsyncs ShadeHaste)
defaultElement constructor attrs children =
  ShadeHaste $ do (_, chlds) <- liftIO (runWriterT (runShadeHaste children))
                  (callbacks, asyncs) <- liftIO mkAsyncs
                  tell (D.singleton
                        (do c <- sequence (D.toList chlds)
                            mcbs <- listenedCallbacks callbacks
                            constructor attrs (catMaybes mcbs) c))
                  return asyncs

voidElement :: (t -> [R.EventHandler] -> IO R.React)
            -> t
            -> ShadeHaste (ElemAsyncs ShadeHaste)
voidElement constructor attrs =
  ShadeHaste $ do (callbacks, asyncs) <- liftIO mkAsyncs
                  tell (D.singleton
                        (do mcbs <- listenedCallbacks callbacks
                            constructor attrs (catMaybes mcbs)))
                  return asyncs

instance ToString JSString where
  toString = fromJSStr

newtype ShadeHaste a = ShadeHaste {
    runShadeHaste :: WriterT (DList (IO R.React)) IO a
    } deriving (Functor, Applicative, Monad)

instance Shade ShadeHaste where
  type Async ShadeHaste = AsyncImpl
  type NativeString ShadeHaste = JSString
  type NativeElem ShadeHaste = Elem
  button = defaultElement R.button
  div = defaultElement R.div
  header = defaultElement R.header
  h1 = defaultElement R.h1
  section = defaultElement R.section
  ul = defaultElement R.ul
  li = defaultElement R.li
  label = defaultElement R.label
  footer = defaultElement R.footer
  span = defaultElement R.span
  strong = defaultElement R.strong
  a = defaultElement R.a
  input = voidElement R.input
  text s = ShadeHaste $ void (tell (D.singleton (R.text s)))
  letElt c = ShadeHaste $ do (s, chlds) <- liftIO (runWriterT (runShadeHaste c))
                             return (s, ShadeHaste (tell chlds >> return s))

listen :: AsyncImpl a -> (a -> IO ()) -> IO ()
listen (AsyncImpl as) callb = mapM_ addCB as
  where
    addCB (AsyncSource (itoa, mv)) = modifyMVar_ mv $ return . ((callb . itoa):)

runClient :: ShadeHaste a -> IO (a, [IO R.React])
runClient c = do
    (s, cs) <- runWriterT (runShadeHaste c)
    return (s, D.toList cs)

renderClient :: Elem -> [IO R.React] -> IO ()
renderClient e rs = do
    relts <- sequence rs
    case relts of
        -- TODO: Just attaching first thing at the toplevel. Defensive div
        -- wrapper? is multiple OK?
        (a:_) -> R.renderComponent e a
        _ -> return ()
