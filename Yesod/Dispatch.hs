{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Yesod.Dispatch
    ( -- * Quasi-quoted routing
      parseRoutes
    , mkYesod
    , mkYesodSub
      -- ** More fine-grained
    , mkYesodData
    , mkYesodSubData
    , mkYesodDispatch
    , mkYesodSubDispatch
      -- ** Path pieces
    , SinglePiece (..)
    , MultiPiece (..)
    , Strings
      -- * Convert to WAI
    , toWaiApp
    , toWaiAppPlain
#if TEST
    , dispatchTestSuite
#endif
    ) where

import Prelude hiding (exp)
import Yesod.Core
import Yesod.Handler
import Yesod.Internal.Dispatch

import Web.Routes.Quasi
import Web.Routes.Quasi.Parse
import Web.Routes.Quasi.TH
import Language.Haskell.TH.Syntax

import qualified Network.Wai as W
import Network.Wai.Middleware.Jsonp
import Network.Wai.Middleware.Gzip

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString as S
import Data.ByteString.Lazy.Char8 ()

import Web.ClientSession
import Data.Char (isUpper)

import Web.Routes (decodePathInfo)

import Control.Monad.IO.Class (liftIO)

#if TEST
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import System.IO.Unsafe
#endif

-- | Generates URL datatype and site function for the given 'Resource's. This
-- is used for creating sites, /not/ subsites. See 'mkYesodSub' for the latter.
-- Use 'parseRoutes' to create the 'Resource's.
mkYesod :: String -- ^ name of the argument datatype
        -> [Resource]
        -> Q [Dec]
mkYesod name = fmap (uncurry (++)) . mkYesodGeneral name [] [] False

-- | Generates URL datatype and site function for the given 'Resource's. This
-- is used for creating subsites, /not/ sites. See 'mkYesod' for the latter.
-- Use 'parseRoutes' to create the 'Resource's. In general, a subsite is not
-- executable by itself, but instead provides functionality to
-- be embedded in other sites.
mkYesodSub :: String -- ^ name of the argument datatype
           -> Cxt
           -> [Resource]
           -> Q [Dec]
mkYesodSub name clazzes =
    fmap (uncurry (++)) . mkYesodGeneral name' rest clazzes True
  where
    (name':rest) = words name

-- | Sometimes, you will want to declare your routes in one file and define
-- your handlers elsewhere. For example, this is the only way to break up a
-- monolithic file into smaller parts. Use this function, paired with
-- 'mkYesodDispatch', to do just that.
mkYesodData :: String -> [Resource] -> Q [Dec]
mkYesodData name res = mkYesodDataGeneral name [] False res

mkYesodSubData :: String -> Cxt -> [Resource] -> Q [Dec]
mkYesodSubData name clazzes res = mkYesodDataGeneral name clazzes True res

mkYesodDataGeneral :: String -> Cxt -> Bool -> [Resource] -> Q [Dec]
mkYesodDataGeneral name clazzes isSub res = do
    let (name':rest) = words name
    (x, _) <- mkYesodGeneral name' rest clazzes isSub res
    let rname = mkName $ "resources" ++ name
    eres <- lift res
    let y = [ SigD rname $ ListT `AppT` ConT ''Resource
            , FunD rname [Clause [] (NormalB eres) []]
            ]
    return $ x ++ y

-- | See 'mkYesodData'.
mkYesodDispatch :: String -> [Resource] -> Q [Dec]
mkYesodDispatch name = fmap snd . mkYesodGeneral name [] [] False

mkYesodSubDispatch :: String -> Cxt -> [Resource] -> Q [Dec]
mkYesodSubDispatch name clazzes = fmap snd . mkYesodGeneral name' rest clazzes True 
  where (name':rest) = words name

mkYesodGeneral :: String -- ^ foundation name
               -> [String] -- ^ parameters for foundation
               -> Cxt -- ^ classes
               -> Bool -- ^ is subsite?
               -> [Resource]
               -> Q ([Dec], [Dec])
mkYesodGeneral name args clazzes isSub res = do
    let name' = mkName name
        args' = map mkName args
        arg = foldl AppT (ConT name') $ map VarT args'
    th' <- mapM thResourceFromResource res
    let th = map fst th'
    w' <- createRoutes th
    let routesName = mkName $ name ++ "Route"
    let w = DataD [] routesName [] w' [''Show, ''Read, ''Eq]
    let x = TySynInstD ''Route [arg] $ ConT routesName

    render <- createRender th
    let x' = InstanceD [] (ConT ''RenderRoute `AppT` ConT routesName)
                [ FunD (mkName "renderRoute") render
                ]

    yd <- mkYesodDispatch' th'
    let master = mkName "master"
    let ctx = if isSub
                then ClassP (mkName "Yesod") [VarT master] : clazzes
                else []
    let ytyp = if isSub
                then ConT ''YesodDispatch `AppT` arg `AppT` VarT master
                else ConT ''YesodDispatch `AppT` arg `AppT` arg
    let y = InstanceD ctx ytyp [FunD (mkName "yesodDispatch") [yd]]
    return ([w, x, x'], [y])

thResourceFromResource :: Resource -> Q (THResource, Maybe String)
thResourceFromResource (Resource n ps atts)
    | all (all isUpper) atts = return ((n, Simple ps atts), Nothing)
thResourceFromResource (Resource n ps [stype, toSubArg]) = do
    let stype' = ConT $ mkName stype
    parse <- [|error "ssParse"|]
    dispatch <- [|error "ssDispatch"|]
    render <- [|renderRoute|]
    tmg <- [|error "ssToMasterArg"|]
    return ((n, SubSite
        { ssType = ConT ''Route `AppT` stype'
        , ssParse = parse
        , ssRender = render
        , ssDispatch = dispatch
        , ssToMasterArg = tmg
        , ssPieces = ps
        }), Just toSubArg)

thResourceFromResource (Resource n _ _) =
    error $ "Invalid attributes for resource: " ++ n

-- | Convert the given argument into a WAI application, executable with any WAI
-- handler. This is the same as 'toWaiAppPlain', except it includes three
-- middlewares: GZIP compression, JSON-P and path cleaning. This is the
-- recommended approach for most users.
toWaiApp :: (Yesod y, YesodDispatch y y) => y -> IO W.Application
toWaiApp y = do
    a <- toWaiAppPlain y
    return $ gzip False
           $ jsonp
             a

-- | Convert the given argument into a WAI application, executable with any WAI
-- handler. This differs from 'toWaiApp' in that it uses no middlewares.
toWaiAppPlain :: (Yesod y, YesodDispatch y y) => y -> IO W.Application
toWaiAppPlain a = do
    key' <- encryptKey a
    return $ toWaiApp' a key'

toWaiApp' :: (Yesod y, YesodDispatch y y)
          => y
          -> Maybe Key
          -> W.Application
toWaiApp' y key' env = do
    let segments =
            case decodePathInfo $ B.unpack $ W.pathInfo env of
                "":x -> x
                x -> x
    liftIO $ print (W.pathInfo env, segments)
    case yesodDispatch y key' segments y id of
        Just app -> app env
        Nothing ->
            case cleanPath y segments of
                Nothing ->
                    case yesodDispatch y key' segments y id of
                        Just app -> app env
                        Nothing -> yesodRunner y y id key' Nothing notFound env
                Just segments' ->
                    let dest = joinPath y (approot y) segments' []
                        dest' =
                            if S.null (W.queryString env)
                                then dest
                                else dest ++ '?' : B.unpack (W.queryString env)
                     in return $ W.responseLBS W.status301
                            [ ("Content-Type", "text/plain")
                            , ("Location", B.pack $ dest')
                            ] "Redirecting"

#if TEST

dispatchTestSuite :: Test
dispatchTestSuite = testGroup "Yesod.Dispatch"
    [ testProperty "encode/decode session" propEncDecSession
    , testProperty "get/put time" propGetPutTime
    ]

propEncDecSession :: [(String, String)] -> Bool
propEncDecSession session' = unsafePerformIO $ do
    key <- getDefaultKey
    now <- getCurrentTime
    let expire = addUTCTime 1 now
    let rhost = B.pack "some host"
    let val = encodeSession key expire rhost session'
    return $ Just session' == decodeSession key now rhost val

propGetPutTime :: UTCTime -> Bool
propGetPutTime t = Right t == runGet getTime (runPut $ putTime t)

instance Arbitrary UTCTime where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return $ addUTCTime (fromRational b)
               $ UTCTime (ModifiedJulianDay a) 0

#endif
