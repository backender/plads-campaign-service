{-# LANGUAGE OverloadedStrings #-}

module Dash (
  MPD (..),
  Period (..),
  AdaptationSet(..),
  Representation(..),
  SegmentTemplate(..),
  Periodable(..),
  defaultVideoFormat,
  videoProfile2160p,
  videoProfile1080p,
  videoProfile720p
) where

import qualified Data.Text          as T
import qualified Data.Time.Clock    as UTC
import           Data.Time.Format
import           Text.XML.Generator


data MPD = MPD {  mpdId                 :: Int
                , mpdPeriods            :: [Period]
                , availabilityStartTime :: T.Text
              }

data Period = Period { perId           :: Int,
                       perStart        :: T.Text,
                       perAdaptionSets :: [AdaptationSet]
                     }

data AdaptationSet = AdaptationSet { asVideoFormat     :: VideoFormat
                                   , asRepresentations :: [Representation]
                                   }

data VideoFormat = VideoFormat { vfMimeType :: T.Text
                               , vfCodecs   :: T.Text
                               }

data Representation = Representation { repVideoProfile :: VideoProfile
                                     , repSegmentTemplate :: SegmentTemplate
                                     }

data VideoProfile = VideoProfile { vpId        :: T.Text,
                                   vpBandwith  :: Int,
                                   vpWidth     :: Int,
                                   vpHeight    :: Int,
                                   vpFrameRate :: Int
                                 }

data SegmentTemplate = SegmentTemplate { stTimescale      :: Int,
                                         stDuration       :: Int,
                                         stStartNumber    :: Int,
                                         stMedia          :: T.Text,
                                         stInitialization :: T.Text
                                       }

type PeriodId = Int
class Periodable a where
  toPeriod :: a -> PeriodId -> Int -> Period

showT :: (Show s) => s -> T.Text
showT = T.pack . show

iso8601Duration :: UTC.UTCTime -> T.Text
iso8601Duration t = T.pack $ "P" ++ formatTime defaultTimeLocale "%s" t

defaultVideoFormat = VideoFormat { vfMimeType = "video/mp4"
                                 , vfCodecs = "avc1.42c00d"
                                 }

videoProfile2160p = VideoProfile { vpId = "2160p12Mbps"
                                , vpBandwith = 12000
                                , vpWidth = 4096
                                , vpHeight = 2160
                                , vpFrameRate = 24
                                }

videoProfile1080p = VideoProfile { vpId = "1080p4800Kbps"
                                , vpBandwith = 4800
                                , vpWidth = 1920
                                , vpHeight = 1080
                                , vpFrameRate = 24
                                }

videoProfile720p = VideoProfile { vpId = "720p4800Kbps"
                                , vpBandwith = 2400
                                , vpWidth = 1280
                                , vpHeight = 720
                                , vpFrameRate = 24
                                }

class XmlConvertible t where
  toXml :: t -> Xml Elem

instance XmlConvertible MPD where
  toXml m = xelem "MPD" $
                 xattr "xmlns:xsi" "http://www.w3.org/2001/XMLSchema-instance"
              <> xattr "xmlns" "urn:mpeg:dash:schema:mpd:2011"
              <> xattr "xsi:schemaLocation" "urn:mpeg:dash:schema:mpd:2011 http://standards.iso.org/ittf/PubliclyAvailableStandards/MPEG-DASH_schema_files/DASH-MPD.xsd"
              <> xattr "type" "dynamic"
              <> xattr "availabilityStartTime" (availabilityStartTime m)
              <> xattr "timeShiftBufferDepth" "PT10M"
              <> xattr "minBufferTime" "PT10S"
              <> xattr "profiles" "urn:mpeg:dash:profile:isoff-live:2011"
              <> xattr "minimumUpdatePeriod" "PT2S"

instance XmlConvertible Period where
  toXml p = xelem "Period" $
                  xattr "id" ((showT . perId) p)
              <>  xattr "start" (perStart p)
              <#> map toXml (perAdaptionSets p)


instance XmlConvertible AdaptationSet where
  toXml as = xelem "AdaptationSet" $
                 xattr "mimeType" (vfMimeType $ asVideoFormat as)
              <> xattr "codecs" (vfCodecs $ asVideoFormat as)
             <#> map toXml (asRepresentations as)

instance XmlConvertible Representation where
  toXml r = xelem "Representation" $
                 xattr "id" (vpId $ repVideoProfile r)
              <> xattr "bandwidth" ((showT . vpBandwith . repVideoProfile) r)
              <> xattr "width" ((showT . vpWidth . repVideoProfile) r)
              <> xattr "height" ((showT . vpHeight . repVideoProfile) r)
              <> xattr "frameRate" ((showT . vpFrameRate . repVideoProfile) r)
             <#> toXml (repSegmentTemplate r)

instance XmlConvertible SegmentTemplate where
  toXml st = xelem "SegmentTemplate" $
                 xattr "timescale" ((showT . stTimescale) st)
              <> xattr "duration" ((showT . stDuration) st)
              <> xattr "startNumber" ((showT . stStartNumber) st)
