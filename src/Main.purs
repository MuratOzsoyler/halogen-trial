module Main where 

import Prelude

import CSS (backgroundColor, lightgray) as CSS
import Control.Monad.State (put)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class.Console (log)
import Effect.Random (randomInt)
import Effect.Timer (setInterval)
import FRP.Event as E
import Halogen (ClassName(..), ComponentHTML, liftEffect)
import Halogen as H
import Halogen.Aff (awaitBody)
import Halogen.HTML as HH
import Halogen.HTML.CSS (style) as CSS
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as HQES
import Halogen.VDom.Driver (runUI)

type State = Maybe Int

data Action = Init | NewInt Int


component :: E.Event Int -> H.Component HH.HTML (Const Unit) Unit Void Aff
component evt = H.mkComponent
    { initialState : const Nothing
    , render: renderer
    , eval: H.mkEval $ H.defaultEval 
        { handleAction = actionHandler 
        , initialize = Just Init
        }
    }
  where
    renderer :: State -> ComponentHTML Action () Aff -- StateAndActionRenderer State Action
    renderer state = HH.div 
        [ HP.class_ $ ClassName "container-fluid"
        , CSS.style $ CSS.backgroundColor CSS.lightgray 
        ]
        [ HH.text $ "Value: " <> show state ]

    actionHandler :: Action -> H.HalogenM State Action () Void Aff Unit -- HandleSimpleAction State Action
    actionHandler = case _ of
        Init -> void $ H.subscribe $ HQES.effectEventSource \emitter -> do
            unsubscribe <- E.subscribe evt (HQES.emit emitter <<< NewInt) {- (log <<< ("Received value:" <> _) <<< show) -}
            pure $ HQES.Finalizer $ unsubscribe
        NewInt i -> put $ Just i

main :: Effect Unit
main = do 
    {event: evt, push} <- E.create  :: Effect (E.EventIO Int)
    launchAff_ do
        body <- awaitBody
        void $ runUI (component evt) unit body

        uns <- liftEffect $ E.subscribe evt (log <<< ("Generated Int: " <> _) <<< show)
        -- uns2 <- liftEffect $ E.subscribe evt (log <<< ("Generated Int2: " <> _) <<< show)

        liftEffect $ void $ setInterval 1000 (push =<< randomInt 10 100)
