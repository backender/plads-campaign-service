{-# LANGUAGE OverloadedStrings #-}

module Dash (
  MPD (..),
  Period (..),
  AdaptationSet(..),
  Representation(..),
  SegmentTemplate(..),
  Periodable(..)
) where

import qualified Data.Text as T
import qualified Data.Time.Clock                    as UTC
import Text.XML.Generator

data MPD = MPD {  mpdId :: Int
                , mpdPeriods :: [Period]
                , availabilityStartTime :: UTC.UTCTime
              }

data Period = Period { perId          :: Int,
                       perStart       :: UTC.UTCTime,
                       perAdaptionSets :: [AdaptationSet]
                     }

data AdaptationSet = AdaptationSet { asMimeType       :: T.Text,
                                     asCodecs         :: T.Text,
                                     asRepresentations :: [Representation]
                                   }
data Representation = Representation { repId              :: T.Text,
                                       repBandwidth       :: Int,
                                       repSegmentTemplate :: SegmentTemplate
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

showT s = (T.pack . show) s

class XmlConvertible t where
  toXml :: t -> Xml Elem

instance XmlConvertible MPD where
  toXml m = xelem "MPD" $
                 xattr "xmlns:xsi" "http://www.w3.org/2001/XMLSchema-instance"
              <> xattr "xmlns" "urn:mpeg:dash:schema:mpd:2011"
              <> xattr "xsi:schemaLocation" "urn:mpeg:dash:schema:mpd:2011 http://standards.iso.org/ittf/PubliclyAvailableStandards/MPEG-DASH_schema_files/DASH-MPD.xsd"
              <> xattr "type" "dynamic"
              <> xattr "availabilityStartTime" ((showT . availabilityStartTime) m)
              <> xattr "timeShiftBufferDepth" "PT10M"
              <> xattr "minBufferTime" "PT10S"
              <> xattr "profiles" "urn:mpeg:dash:profile:isoff-live:2011"
              <> xattr "minimumUpdatePeriod" "PT2S"

instance XmlConvertible Period where
  toXml p = xelem "Period" $
                  xattr "id" ((showT . perId) p)
              <>  xattr "start" ((show . perStart) p)
              <#> map toXml (perAdaptionSets p)


instance XmlConvertible AdaptationSet where
  toXml as = xelem "AdaptationSet" $
                 xattr "mimeType" ((showT . asMimeType) as)
              <> xattr "codecs" ((showT . asCodecs) as)
             <#> map toXml (asRepresentations as)

instance XmlConvertible Representation where
  toXml r = xelem "Representation" $
                 xattr "id" ((showT . repId) r)
              <> xattr "bandwidth" ((showT . repBandwidth) r)
             <#> toXml (repSegmentTemplate r)

instance XmlConvertible SegmentTemplate where
  toXml st = xelem "SegmentTemplate" $
                 xattr "timescale" ((showT . stTimescale) st)
              <> xattr "duration" ((showT . stDuration) st)
              <> xattr "startNumber" ((showT . stStartNumber) st)
