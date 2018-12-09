{-# LANGUAGE OverloadedStrings #-}
module Level02.Core (runApp, app) where

import           Network.Wai              (Application, Request, Response,
                                           pathInfo, requestMethod, responseLBS,
                                           strictRequestBody)
import           Network.Wai.Handler.Warp (run)

import           Network.HTTP.Types       (Status, hContentType, status200,
                                           status400, status404)

import qualified Data.ByteString.Lazy     as LBS
import Data.ByteString.Lazy.Char8         (pack)

import           Data.Either              (either)

import           Data.Text                (Text)
import           Data.Text.Encoding       (decodeUtf8)

import           Level02.Types           (ContentType(..), Error(..), RqType(..),
                                           mkCommentText, mkTopic,
                                           renderContentType)

-- |-------------------------------------------|
-- |- Don't start here, go to Level02.Types!  -|
-- |-------------------------------------------|

-- | Some helper functions to make our lives a little more DRY.
mkResponse
  :: Status
  -> ContentType
  -> LBS.ByteString
  -> Response
mkResponse s ct = responseLBS s [("Content-Type", renderContentType ct)]

resp200
  :: ContentType
  -> LBS.ByteString
  -> Response
resp200 = mkResponse status200

resp404
  :: ContentType
  -> LBS.ByteString
  -> Response
resp404 = mkResponse status404

resp400
  :: ContentType
  -> LBS.ByteString
  -> Response
resp400 = mkResponse status400

-- |----------------------------------------------------------------------------------
-- These next few functions will take raw request information and construct         --
-- one of our types.                                                                --
--                                                                                  --
-- By breaking out these smaller functions, we're able to isolate our               --
-- validation requirements into smaller components that are simpler to maintain     --
-- and verify. It also allows for greater reuse and it also means that              --
-- validation is not duplicated across the application, maybe incorrectly.          --
--------------------------------------------------------------------------------------

mkAddRequest
  :: Text
  -> LBS.ByteString
  -> Either Error RqType
mkAddRequest t ct = AddRq <$> mkTopic t <*> mkCommentText (lazyByteStringToStrictText ct)
  where
    -- This is a helper function to assist us in going from a Lazy ByteString, to a Strict Text
    lazyByteStringToStrictText =
      decodeUtf8 . LBS.toStrict

mkViewRequest
  :: Text
  -> Either Error RqType
mkViewRequest = (ViewRq <$>) . mkTopic

mkListRequest
  :: Either Error RqType
mkListRequest = pure ListRq

-- |----------------------------------
-- end of RqType creation functions --
--------------------------------------

mkErrorResponse
  :: Error
  -> Response
mkErrorResponse EmptyTopic = resp404 Plain "Topic was not found"
mkErrorResponse EmptyComment = resp400 Plain "Comments can not be empty"
mkErrorResponse NotFound = resp404 Plain "Resource not found"

-- | Use our ``RqType`` helpers to write a function that will take the input
-- ``Request`` from the Wai library and turn it into something our application
-- cares about.
mkRequest
  :: Request
  -> IO ( Either Error RqType )
mkRequest req = case (requestMethod req, pathInfo req) of
  ("POST", [t, "add"]) -> getCommentFromRequest t
  ("GET", [t, "view"]) -> pure $ mkViewRequest t
  ("GET", ["list"]) -> pure $ mkListRequest
  _ -> pure $ Left NotFound
  where
    getCommentFromRequest t = mkAddRequest t <$> strictRequestBody req

  -- Remembering your pattern-matching skills will let you implement the entire
  -- specification in this function.

-- | If we find that we need more information to handle a request, or we have a
-- new type of request that we'd like to handle then we update the ``RqType``
-- structure and the compiler will let us know which parts of our application
-- are affected.
--
-- Reduction of concerns such that each section of the application only deals
-- with a small piece is one of the benefits of developing in this way.
--
-- For now, return a made-up value for each of the responses as we don't have
-- any persistent storage. Plain text responses that contain "X not implemented
-- yet" should be sufficient.

toByteString :: Show a => a -> LBS.ByteString
toByteString = pack . show

handleRequest
  :: RqType
  -> Either Error Response
handleRequest r@(AddRq _ _) = pure $
  resp200 Plain ("Adding comments not implemented yet. " <> toByteString r)
handleRequest r@(ViewRq _) = pure $
  resp200 Plain ("viewing topics not implemented yet. " <> toByteString r)
handleRequest ListRq = pure $
  resp200 Plain "listing topics not implemented yet"

-- | Reimplement this function using the new functions and ``RqType`` constructors as a guide.
app
  :: Application
app r cb = do
  request <- mkRequest r
  cb $ either mkErrorResponse id (handleRequest =<< request)

runApp :: IO ()
runApp = run 3000 app
