module Dash (
  MPD (..),
  Period (..)
) where

import qualified Data.Text as T

data MPD = MPD { mpdId :: Int}

data Period = Period { perId          :: Int,
                       perStart       :: T.Text,
                       perDuration    :: Int,
                       perAdaptionSet :: AdaptationSet
                     }

data AdaptationSet = AdaptationSet { asMimeType       :: T.Text,
                                     asCodecs         :: T.Text,
                                     asRepresentation :: Representation
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
