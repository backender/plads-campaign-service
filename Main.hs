{-# LANGUAGE OverloadedStrings #-}

import           Data.Maybe
import qualified Data.Text                          as T
import qualified Data.Time.Clock                    as UTC

import           Database.MySQL.Simple
import           Database.MySQL.Simple.QueryResults
import           Database.MySQL.Simple.Result

import           Dash

data Display = Display { dId :: Integer } deriving (Show)

data CampaignInfo = CampaignInfo { cid     :: Int,
                                   display :: Int,
                                   start   :: UTC.UTCTime,
                                   end     :: UTC.UTCTime,
                                   src     :: T.Text,
                                   mpd     :: Maybe T.Text
                                 } deriving (Show)

instance QueryResults Display where
    convertResults [fa] [va] = Display dId
        where dId = convert fa va
    convertResults fs vs  = convertError fs vs 1

instance QueryResults CampaignInfo where
    convertResults [fa,fb,fc,fd,fe,ff] [va,vb,vc,vd,ve,vf] =
            CampaignInfo cid display start end src mpd
        where cid = convert fa va
              display = convert fb vb
              start = convert fc vc
              end = convert fd vd
              src = convert fe ve
              mpd = convert ff vf
    convertResults fs vs  = convertError fs vs 6

getDisplays :: Connection -> IO [Display]
getDisplays conn = query_ conn "select * from display"

findCampaignInfo :: Connection -> IO [CampaignInfo]
findCampaignInfo conn = query_ conn
        "select c.id, c.display_id, c.start, c.end, m.src, m.mpd_url from campaign c left join media m on m.id = c.media_id"

filterNoMpd = filter (\ci -> isJust $ mpd ci )
--filterStartEnd cis now = filter (\ci -> (start ci >= now) && (end ci <= now)) cis

filterCampaignInfos :: [CampaignInfo] -> [CampaignInfo]
filterCampaignInfos = filterNoMpd  -- . filterStartEnd

createPeriod :: CampaignInfo -> [Period]
createPeriod ci = []

main :: IO ()
main = print "test"

-- getCampaigns monitor         -- Monitor -> [Campaign]
--  . filterCampaign           -- [Campaign] -> [Campaign]
--    . map createPeriod         -- Campaign -> [Period]
--      . createMpd monitor      -- [Period] -> Mpd
--        . updateStream         -- Mpd -> IO
