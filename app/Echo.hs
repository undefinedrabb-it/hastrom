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
import Data.Aeson.Types (Parser)
import Data.Aeson.Types qualified as AesonT
import Data.ByteString qualified as BS
import Data.ByteString.Lazy.Char8 qualified as BSL8
import Data.Text qualified as T
import GHC.Generics (Generic)
import System.IO (hFlush, hPrint, hPutStrLn, stderr, stdin, stdout)

data Payload
  = Echo
      { echo :: T.Text
      }
  | EchoOk
      { echo :: T.Text
      }
  | Init
      { node_id :: T.Text,
        node_ids :: [T.Text]
      }
  | InitOk
  | Error
      { code :: Int,
        text :: T.Text
      }
  deriving (Generic, Show)

data MsgBody = MsgBody
  { msg_id :: Int,
    in_reply_to :: Maybe Int,
    payload :: Payload
  }
  deriving (Generic, Show)

data Msg = Msg
  { src :: T.Text,
    dest :: T.Text,
    body :: MsgBody
  }
  deriving (Generic, Show, FromJSON, ToJSON)

instance ToJSON MsgBody where
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
      payloadToPairs (EchoOk {echo}) =
        [ toTypePair "echo_ok",
          "echo" .= echo
        ]
      payloadToPairs (Init {node_id, node_ids}) =
        [ toTypePair "init",
          "node_id" .= node_id,
          "node_ids" .= node_ids
        ]
      payloadToPairs (InitOk {}) =
        [ toTypePair "init_ok"
        ]
      payloadToPairs (Error {code, text}) =
        [ toTypePair "error",
          "code" .= code,
          "text" .= text
        ]

instance FromJSON MsgBody where
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
        "echo_ok" -> EchoOk <$> obj .: "echo"
        "init" -> Init <$> obj .: "node_id" <*> obj .: "node_ids"
        "init_ok" -> pure InitOk
        "error" -> Error <$> obj .: "code" <*> obj .: "text"
        _ -> fail "payload type not found"

data State = State
  { node_id :: T.Text,
    message_id :: Int
  }
  deriving (Show)

ff :: State -> T.Text -> Int -> Payload -> Msg
ff s' dest in_reply_to payload =
  Msg
    { src = s'.node_id,
      dest,
      body = MsgBody {msg_id = s'.message_id, in_reply_to = Just in_reply_to, payload}
    }

pl :: Payload -> Payload
pl (Echo {echo}) = EchoOk {echo}
pl (Init {node_id, node_ids}) = InitOk {}

lp :: State -> IO b
lp state = do
  s <- getLine
  hPutStrLn stderr s
  let (state', res) = z state . BSL8.pack $ s
  putStrLn . BSL8.unpack $ res
  hFlush stdout
  lp state'
  where
    z :: State -> BSL8.ByteString -> (State, BSL8.ByteString)
    z state s =
      let req = decode s :: Maybe Msg
       in let (state', res) = case (state, req) of
                (s@(State {message_id}), Just Msg {src, body = MsgBody {msg_id, payload}}) ->
                  let s' = s {message_id = 1 + message_id}
                   in let res = ff s' src msg_id . pl $ payload
                       in (s', res)
                _ -> (state, errorMsg)
           in (state', encode res)
    errorMsg =
      (Msg {src = "0", dest = "0", body = MsgBody {msg_id = 0, in_reply_to = Nothing, payload = Error {code = 0, text = "error"}}})

main :: IO ()
main = do
  let state = State {node_id = "node1", message_id = 0}
  lp state
