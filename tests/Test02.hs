{-# LANGUAGE OverloadedStrings #-}
module Main where

-- | **REMINDER**
-- This level is not an isolated module to complete. This level exists as one
-- starting module: `test/Test.hs`. Which you are to import your most recently
-- completed `Application` to be tested.
--
-- As you progress through the course, you are encouraged to return to this
-- `test/Test.hs` and update it so you're able to be confident that your
-- application will behave as you expect. You may also write your tests before
-- you write your functions, this can be useful when trying to think through a
-- problem.

-- | This is the only location for tests as you progress through the course.

-- | This module starts our very sparse. There are only the imports required to
-- have these initial tests work. Additional tests are for you to build. and may
-- require you to import other modules as you require.
--
-- As you progress through the levels, the initialisation of the 'app' will
-- become more complicated as more components are introduced. Part of the
-- exercise is to work out how to integrate your application with this testing
-- framework.

-- | 'tasty' takes care of managing all of our test cases, running them,
-- checking results and then providing us with a report.
import           Test.Tasty         (defaultMain, testGroup, TestTree)

-- | 'tasty-wai' makes it easier to create requests to submit to our
-- application, and provides some helper functions for checking our assertions.
import           Test.Tasty.Wai     (assertBody, assertBodyContains, assertStatus', assertContentType, get, post, testWai)
import           Test.Tasty.HUnit     (assertBool, testCase, (@?=))
import           Level02.Types (mkTopic, mkCommentText, getTopic, getCommentText, Error(..))

-- | For running unit tests for individual functions, we have included the
-- 'tasty-hunit' package. More information is available on the Hackage page:
-- https://hackage.haskell.org/package/tasty-hunit.
--
-- import qualified Test.Tasty.HUnit as HU
--

import           Network.HTTP.Types as HTTP

-- | This import is provided for you so you can check your work from Level02. As
-- you move forward, come back and import your latest 'Application' so that you
-- can test your work as you progress.
import qualified Level02.Core       as Core
domainTypesTests :: TestTree
domainTypesTests = testGroup "domain types tests"
  [
    testCase "mkTopic with empty text is a failure" $
      mkTopic "" @?= Left EmptyTopic

  , testCase "mkComment with empty text is a failure" $
      mkCommentText "" @?= Left EmptyComment

  , testCase "mkTopic with text makes a topic" $
      getTopic <$> mkTopic "fudge" @?= Right "fudge"

  , testCase "mkCommentText with text makes a comment" $
      getCommentText <$> mkCommentText "i like fudge!" @?= Right "i like fudge!"
  ]

waiTests :: TestTree
waiTests = testGroup "wai tests"
  [
    testWai Core.app "Root path returns 404" $
      get "/" >>= assertStatus' HTTP.status404

  , testWai Core.app "Non-handled path returns 404" $
      get "/foo/bar" >>= assertStatus' HTTP.status404

  , testWai Core.app "Specifying empty topic returns 404" $ do
      resp <- post "//add" "Comment"
      assertStatus' HTTP.status404 resp
      assertBody "Topic was not found" resp

  , testWai Core.app "Adding empty comment returns 400" $ do
      resp <- post "fudge/add" ""
      assertStatus' HTTP.status400 resp
      assertBody "Comments can not be empty" resp

  , testWai Core.app "Listing topics returns 200" $ do
      resp <- get "list"
      assertStatus' HTTP.status200 resp

  , testWai Core.app "Viewing a topic returns 200" $ do
      resp <- get "fudge/view"
      assertStatus' HTTP.status200 resp
      assertContentType "text/plain" resp

  , testWai Core.app "Adding non-empty comment succeeds with 200" $ do
      resp <- post "fudge/add" "this is a comment"
      assertStatus' HTTP.status200 resp
      assertBodyContains "fudge" resp
  ]

main :: IO ()
main = defaultMain $ testGroup "Applied FP Course - Tests" [
    waiTests, domainTypesTests
  ]


