module Main where

import Data.Aeson
  ( FromJSON (parseJSON),
    Object,
    ToJSON (toJSON),
    Value (Object),
    decode,
    encode,
    fromJSON,
    json,
    json',
    object,
    withObject,
    (.:),
    (.:?),
    (.=),
  )
import Data.Aeson.Types qualified as AesonT
import Data.ByteString qualified as BS
import Data.ByteString.Lazy.Char8 qualified as BSL8
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Polysemy
import Polysemy.Input
import Polysemy.Output
import System.IO (hFlush, hPrint, hPutStrLn, stderr, stdin, stdout)

data Req
  = Topology {topology :: Map.Map T.Text T.Text}
  | Echo
      { echo :: T.Text
      }
  | Init
      { node_id :: T.Text,
        node_ids :: [T.Text]
      }
  deriving (Generic, Show)

data Res
  = TopologyOk
  | EchoOk
      { echo :: T.Text
      }
  | InitOk
  | Error
      { code :: Int,
        text :: T.Text
      }
  deriving (Generic, Show)

data MsgBody p = MsgBody
  { msg_id :: Int,
    in_reply_to :: Maybe Int,
    payload :: p
  }
  deriving (Generic, Show)

data Msg p = Msg
  { src :: T.Text,
    dest :: T.Text,
    body :: MsgBody p
  }
  deriving (Generic, Show)

instance ToJSON (Msg Res)

instance ToJSON (Msg Req)

instance FromJSON (Msg Res)

instance FromJSON (Msg Req)

instance ToJSON (MsgBody Res) where
  toJSON = object . msgBodyToPairs
    where
      msgBodyToPairs (MsgBody {msg_id, in_reply_to, payload}) =
        [ "msg_id" .= msg_id,
          "in_reply_to" .= in_reply_to
        ]
          <> payloadToPairs payload

      toTypePair s = ("type", s)
      payloadToPairs (EchoOk {echo}) =
        [ toTypePair "echo_ok",
          "echo" .= echo
        ]
      payloadToPairs (TopologyOk) =
        [ toTypePair "topology_ok"
        ]
      payloadToPairs (InitOk) =
        [ toTypePair "init_ok"
        ]
      payloadToPairs (Error {code, text}) =
        [ toTypePair "error",
          "code" .= code,
          "text" .= text
        ]

instance ToJSON (MsgBody Req) where
  toJSON = object . msgBodyToPairs
    where
      msgBodyToPairs (MsgBody {msg_id, in_reply_to, payload}) =
        [ "msg_id" .= msg_id,
          "in_reply_to" .= in_reply_to
        ]
          <> payloadToPairs payload

      toTypePair s = ("type", s)
      payloadToPairs (Echo {echo}) =
        [ toTypePair "echo",
          "echo" .= echo
        ]
      payloadToPairs (Init {node_id, node_ids}) =
        [ toTypePair "init",
          "node_id" .= node_id,
          "node_ids" .= node_ids
        ]
      payloadToPairs (Topology {topology}) =
        [ toTypePair "topology",
          "topology" .= topology
        ]

instance FromJSON (MsgBody Res) where
  parseJSON = withObject "Payload" $ \obj ->
    do
      msg_id <- obj .: "msg_id"
      in_reply_to <- obj .:? "in_reply_to"
      _type <- obj .: "type" :: AesonT.Parser T.Text

      payload <- payloadFrom _type obj

      pure $ MsgBody {msg_id, in_reply_to, payload}
    where
      payloadFrom _type obj = case _type of
        "echo_ok" -> EchoOk <$> obj .: "echo"
        "init_ok" -> pure InitOk
        "topology_ok" -> pure TopologyOk
        "error" -> Error <$> obj .: "code" <*> obj .: "text"
        _ -> fail "payload type not found"

instance FromJSON (MsgBody Req) where
  parseJSON = withObject "Payload" $ \obj ->
    do
      msg_id <- obj .: "msg_id"
      in_reply_to <- obj .:? "in_reply_to"
      _type <- obj .: "type" :: AesonT.Parser T.Text

      payload <- payloadFrom _type obj

      pure $ MsgBody {msg_id, in_reply_to, payload}
    where
      payloadFrom _type obj = case _type of
        "echo" -> Echo <$> obj .: "echo"
        "init" -> Init <$> obj .: "node_id" <*> obj .: "node_ids"
        "topology" -> Topology <$> obj .: "topology"
        _ -> fail "payload type not found"

data State = State
  { node_id :: T.Text,
    message_id :: Int,
    topology :: Map.Map T.Text T.Text
  }
  deriving (Show)

data Logger m a where
  LogInfo :: Show s => s -> Logger m ()

makeSem ''Logger

loggerToIO :: Member (Embed IO) r => Sem (Logger ': r) a -> Sem r a
loggerToIO = interpret \case
  LogInfo s -> embed . hPrint stderr $ s

data Teletype m a where
  ReadTTY :: Teletype m String
  WriteTTY :: String -> Teletype m ()

makeSem ''Teletype

teletypeToIO :: Member (Embed IO) r => Sem (Teletype ': r) a -> Sem r a
teletypeToIO = interpret \case
  ReadTTY -> embed getLine
  WriteTTY msg -> embed $ do
    putStrLn msg
    hFlush stdout

data Event = SetNodeId {id :: T.Text} | IncMsg

data Node m a where
  SendMsg :: Maybe (Msg Res) -> Node m ()
  SendErrMsg :: Node m ()
  NewNode :: Node m State
  DispatchEvent :: Event -> State -> Node m State

makeSem ''Node

pl :: Member Node r => State -> Req -> Sem r (State, Res)
pl state (Echo {echo}) = pure (state, EchoOk {echo})
pl state (Init {node_id, node_ids}) = do
  state' <- dispatchEvent (SetNodeId node_id) state
  pure (state', InitOk)
pl state (Topology {topology}) = pure (state, TopologyOk)

errorMsg =
  (Msg {src = "0", dest = "0", body = MsgBody {msg_id = 0, in_reply_to = Nothing, payload = Error {code = 0, text = "error"}}})

nodeToIO :: Member Teletype r => Member (Embed IO) r => Sem (Node ': r) a -> Sem r a
nodeToIO = interpret \case
  SendMsg s -> do writeTTY . BSL8.unpack $ encode $ fromMaybe errorMsg s
  NewNode -> do
    pure $ State {node_id = "", message_id = 0, topology = Map.empty}
  DispatchEvent SetNodeId {id} state -> do
    let state' = state {node_id = id} :: State
    pure state'
  DispatchEvent IncMsg state -> do
    let state' = state {message_id = state.message_id + 1} :: State
    pure state'

lpp :: Member Node r => State -> Maybe (Msg Req) -> Sem r State
lpp state Nothing = do
  sendMsg Nothing
  pure state
lpp state (Just msg) = do
  state <- dispatchEvent IncMsg state
  (state', payload) <- pl state msg.body.payload
  sendMsg $ Just $ ms msg state' payload
  pure state'
  where
    ms msg state' payload =
      Msg
        { src = state'.node_id,
          dest = msg.src,
          body =
            MsgBody
              { msg_id = state'.message_id,
                in_reply_to = Just msg.body.msg_id,
                payload
              }
        }

lp :: Members '[Logger, Teletype, Node] r => State -> Sem r ()
lp state = do
  s <- BSL8.pack <$> readTTY
  logInfo s
  let req = decode s :: Maybe (Msg Req)
  state' <- lpp state req
  lp state'

main :: IO ()
main =
  runM . loggerToIO . teletypeToIO . nodeToIO $ do
    state <- newNode
    lp state
