{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad (when)
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
import Data.Foldable (traverse_)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Polysemy
import Polysemy.Input
import Polysemy.Output
import System.IO (hFlush, hPrint, hPutStrLn, stderr, stdin, stdout)

newtype TraceBody = TraceBody {_type :: T.Text} deriving (Show)

instance FromJSON (MsgBody TraceBody) where
  parseJSON = withObject "Payload" $ \obj ->
    do
      msg_id <- obj .:? "msg_id"
      in_reply_to <- obj .:? "in_reply_to"
      _type <- obj .: "type" :: AesonT.Parser T.Text
      pure $ MsgBody {msg_id, in_reply_to, payload = TraceBody {_type}}

instance ToJSON (MsgBody TraceBody) where
  toJSON (MsgBody {msg_id, in_reply_to, payload = TraceBody {_type}}) =
    object
      [ "msg_id" .= msg_id,
        "in_reply_to" .= in_reply_to,
        "type" .= _type
      ]

data Req
  = Init
      { node_id :: T.Text,
        node_ids :: [T.Text]
      }
  | Echo
      { echo :: T.Text
      }
  | Broadcast {message :: Int}
  | Read
  | Topology {topology :: Maybe (Map.Map T.Text [T.Text])}
  deriving (Generic, Show)

data Res
  = Error
      { code :: Int,
        text :: T.Text
      }
  | InitOk
  | EchoOk
      { echo :: T.Text
      }
  | BroadcastOk
  | ReadOk {messages :: [Int]}
  | TopologyOk
  deriving (Generic, Show)

data MsgBody p = MsgBody
  { msg_id :: Maybe Int,
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

instance ToJSON (Msg TraceBody)

instance FromJSON (Msg TraceBody)

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
      payloadToPairs (Error {code, text}) =
        [ toTypePair "error",
          "code" .= code,
          "text" .= text
        ]
      payloadToPairs InitOk =
        [ toTypePair "init_ok"
        ]
      payloadToPairs (EchoOk {echo}) =
        [ toTypePair "echo_ok",
          "echo" .= echo
        ]
      payloadToPairs BroadcastOk =
        [ toTypePair "broadcast_ok"
        ]
      payloadToPairs (ReadOk {messages}) =
        [ toTypePair "read_ok",
          "messages" .= messages
        ]
      payloadToPairs TopologyOk =
        [ toTypePair "topology_ok"
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
      payloadToPairs (Init {node_id, node_ids}) =
        [ toTypePair "init",
          "node_id" .= node_id,
          "node_ids" .= node_ids
        ]
      payloadToPairs (Echo {echo}) =
        [ toTypePair "echo",
          "echo" .= echo
        ]
      payloadToPairs (Broadcast {message}) =
        [ toTypePair "broadcast",
          "message" .= message
        ]
      payloadToPairs Read =
        [toTypePair "read"]
      payloadToPairs (Topology {topology}) =
        [ toTypePair "topology",
          "topology" .= topology
        ]

instance FromJSON (MsgBody Res) where
  parseJSON = withObject "Payload" $ \obj ->
    do
      msg_id <- obj .:? "msg_id"
      in_reply_to <- obj .:? "in_reply_to"
      _type <- obj .: "type" :: AesonT.Parser T.Text

      payload <- payloadFrom _type obj

      pure $ MsgBody {msg_id, in_reply_to, payload}
    where
      payloadFrom _type obj = case _type of
        "error" -> Error <$> obj .: "code" <*> obj .: "text"
        "init_ok" -> pure InitOk
        "echo_ok" -> EchoOk <$> obj .: "echo"
        "broadcast_ok" -> pure BroadcastOk
        "read_ok" -> ReadOk <$> obj .: "messages"
        "topology_ok" -> pure TopologyOk
        _ -> fail "payload type not found"

instance FromJSON (MsgBody Req) where
  parseJSON = withObject "Payload" $ \obj ->
    do
      msg_id <- obj .:? "msg_id"
      in_reply_to <- obj .:? "in_reply_to"
      _type <- obj .: "type" :: AesonT.Parser T.Text

      payload <- payloadFrom _type obj

      pure $ MsgBody {msg_id, in_reply_to, payload}
    where
      payloadFrom _type obj = case _type of
        "init" -> Init <$> obj .: "node_id" <*> obj .: "node_ids"
        "echo" -> Echo <$> obj .: "echo"
        "broadcast" -> Broadcast <$> obj .: "message"
        "read" -> pure Read
        "topology" -> Topology <$> obj .: "topology"
        _ -> fail "payload type not found"

data State = State
  { node_id :: T.Text,
    message_id :: Int,
    messages :: [Int],
    topology :: Map.Map T.Text [T.Text]
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

data Event
  = IncMsg
  | Gossip {about :: Int, gotFrom :: T.Text}
  | AddMsg {msg :: Int}
  | SetNodeId {id :: T.Text}

data Node m a where
  SendMsg :: Maybe (Msg Res) -> Node m ()
  SendErrMsg :: Node m ()
  NewNode :: Node m State
  DispatchEvent :: Event -> State -> Node m State

makeSem ''Node

pl :: Members '[Node, Logger] r => State -> Msg Req -> Sem r (State, Maybe Res)
pl state (Msg {body = MsgBody {payload = (Init {node_id, node_ids})}}) = do
  state' <- dispatchEvent (SetNodeId node_id) state
  pure (state', Just InitOk)
pl state (Msg {body = MsgBody {payload = (Echo {echo})}}) = do
  pure (state, Just EchoOk {echo})
pl state (Msg {src = src, body = MsgBody {msg_id, payload = (Broadcast {message})}}) = do
  state' <-
    if message `notElem` state.messages
      then do
        state' <- dispatchEvent (AddMsg message) state
        dispatchEvent (Gossip {about = message, gotFrom = src}) state'
      else pure state
  if isNothing msg_id
    then pure (state', Nothing)
    else pure (state', Just BroadcastOk)
pl state (Msg {body = MsgBody {payload = Read}}) = do
  pure (state, Just ReadOk {messages = state.messages})
pl state (Msg {body = MsgBody {payload = (Topology {topology})}}) = do
  pure (state {topology = fromMaybe Map.empty topology}, Just TopologyOk)

errorMsg =
  (Msg {src = "0", dest = "0", body = MsgBody {msg_id = Just 0, in_reply_to = Nothing, payload = Error {code = 0, text = "error"}}})

nodeToIO :: Member Teletype r => Member (Embed IO) r => Sem (Node ': r) a -> Sem r a
nodeToIO = interpret \case
  SendMsg s -> do
    send $ fromMaybe errorMsg s
  NewNode -> do
    pure $ State {node_id = "", message_id = 0, topology = Map.empty, messages = []}
  DispatchEvent SetNodeId {id} state -> do
    let state' = state {node_id = id} :: State
    pure state'
  DispatchEvent IncMsg state -> do
    let state' = state {message_id = state.message_id + 1} :: State
    pure state'
  DispatchEvent AddMsg {msg} state -> do
    let state' = state {messages = msg : state.messages} :: State
    pure state'
  DispatchEvent Gossip {about, gotFrom} state -> do
    let neighbours = Map.findWithDefault [] state.node_id state.topology |> filter (/= gotFrom)
    traverse_ (send . msg) neighbours
    pure state
    where
      msg s = Just $ Msg {src = state.node_id, dest = s, body = MsgBody {msg_id = Nothing, in_reply_to = Nothing, payload = Broadcast {message = about}}}
  where
    send s = do
      writeTTY . BSL8.unpack $ encode s

(|>) = flip ($)

lpp :: Members '[Node, Logger] r => State -> Maybe (Msg Req) -> Sem r State
lpp state Nothing = do
  sendMsg Nothing
  pure state
lpp state (Just msg) = do
  state <- dispatchEvent IncMsg state
  (state', payload) <- pl state msg
  case payload of
    Just payload -> do
      sendMsg $ Just $ msgCtor msg state' payload
      pure state'
    Nothing -> pure state'

msgCtor :: Msg Req -> State -> Res -> Msg Res
msgCtor msg state' payload =
  Msg
    { src = state'.node_id,
      dest = msg.src,
      body =
        MsgBody
          { msg_id = Just state'.message_id,
            in_reply_to = msg.body.msg_id,
            payload
          }
    }

lp :: Members '[Logger, Teletype, Node] r => State -> Sem r ()
lp state = do
  s <- BSL8.pack <$> readTTY
  logInfo s
  let req = decode s :: Maybe (Msg Req)
  state' <-
    if isNothing req
      then do
        logInfo $ "Invalid request " <> s
        pure state
      else do
        lpp state req
  lp state'

main :: IO ()
main =
  runM . loggerToIO . teletypeToIO . nodeToIO $ do
    state <- newNode
    lp state
