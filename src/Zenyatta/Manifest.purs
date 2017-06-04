-- | TODO: extract this
module Zenyatta.Manifest
    ( main
    ) where


-------------------------------------------------------------------------------
import Zenyatta.Prelude
import CSS as CSS
import Data.Argonaut.Encode as AE
import Data.String as S
import Data.Argonaut (jsonEmptyObject, (:=), (~>))
import Data.Argonaut.Core (stringify)
import Data.MediaType (MediaType)
import Data.Newtype (unwrap)
-------------------------------------------------------------------------------


main :: forall eff. Eff ( console :: CONSOLE | eff) Unit
main = log (stringify (AE.encodeJson manifest))


-------------------------------------------------------------------------------
manifest :: Manifest
manifest = Manifest
  { name:  "Zenyatta"
  , shortName:  "Zenyatta"
  , startURL:  URL "/timer"
  , dir:  LeftToRight
  , display:  Fullscreen -- probably
  , backgroundColor:  Nothing
  , description:  "A meditation timer."
  , icons:  []
  , lang:  Just (Language "en-US")
  , orientation:  Just Portrait
  , preferRelatedApplications:  Nothing
  , scope:  Nothing
  , themeColor:  Nothing
  , relatedApplications:  []
  }

-------------------------------------------------------------------------------
--TODO: figure out which are optional
newtype Manifest = Manifest
  { name :: String
  , shortName :: String
  , startURL :: URL
  , dir :: Direction
  , display :: Display
  , backgroundColor :: Maybe CSS.Color
  , description :: String
  , icons :: Array Icon
  , lang :: Maybe Language
  , orientation :: Maybe Orientation
  , preferRelatedApplications :: Maybe Boolean
  , scope :: Maybe URL
  , themeColor :: Maybe CSS.Color
  , relatedApplications :: Array RelatedApplication
  }


--TODO: omit missing keys
instance encodeJsonManifest :: AE.EncodeJson Manifest where
  encodeJson (Manifest o) =
       "name" := o.name
    ~> "short_name" := o.shortName
    ~> "start_url" := o.startURL
    ~> "dir" := o.dir
    ~> "description" := o.description
    ~> "icons" := o.icons
    ~> "lang" := o.lang
    ~> "orientation" := o.orientation
    ~> "prefer_related_applications" := o.preferRelatedApplications
    ~> "scope" := o.scope
    ~> "theme_color" := (CSS.toHexString <$> o.themeColor)
    ~> "related_applications" := o.relatedApplications
    ~> jsonEmptyObject


-------------------------------------------------------------------------------
data Orientation = AnyOrientation
                 | Natural
                 | Landscape
                 | LandscapePrimary
                 | LandscapeSecondary
                 | Portrait
                 | PortraitPrimary
                 | PortraitSecondary


instance encodeJsonOrientation :: AE.EncodeJson Orientation where
  encodeJson AnyOrientation     = AE.encodeJson "any"
  encodeJson Natural            = AE.encodeJson "natural"
  encodeJson Landscape          = AE.encodeJson "landscape"
  encodeJson LandscapePrimary   = AE.encodeJson "landscape_primary"
  encodeJson LandscapeSecondary = AE.encodeJson "landscape_secondary"
  encodeJson Portrait           = AE.encodeJson "portrait"
  encodeJson PortraitPrimary    = AE.encodeJson "portrait_primary"
  encodeJson PortraitSecondary  = AE.encodeJson "portrait_secondary"


-------------------------------------------------------------------------------
data Direction = RightToLeft
               | LeftToRight
               | AutoDirection


instance encodeJsonDirection :: AE.EncodeJson Direction where
  encodeJson RightToLeft   = AE.encodeJson "rtl"
  encodeJson LeftToRight   = AE.encodeJson "ltr"
  encodeJson AutoDirection = AE.encodeJson "auto"


-------------------------------------------------------------------------------
data Display = Fullscreen
             | Standalone
             | MinimalUI
             | Browser


-------------------------------------------------------------------------------
newtype Icon = Icon
  { src :: URL
  , mediaType :: Maybe MediaType -- or is it mime
  , sizes :: NonEmpty Array IconSize
  }


instance encodeJson :: AE.EncodeJson Icon where
  encodeJson (Icon o) =
       "src" := o.src
    ~> "media_type" := (unwrap <$> o.mediaType)
    ~> "sizes" := S.joinWith " " (iconSizeString <$> case o.sizes of NonEmpty x xs -> x:xs)
    ~> jsonEmptyObject


-------------------------------------------------------------------------------
newtype URL = URL String

derive newtype instance encodeJsonURL :: AE.EncodeJson URL


-------------------------------------------------------------------------------
-- | e.g. IANA language subtag, e.g. en-US
newtype Language = Language String


derive newtype instance encodeJsonLanguage :: AE.EncodeJson Language


-------------------------------------------------------------------------------
data IconSize = AnyIconSize
              | Pixels Int Int



instance encodeJsonIconSize :: AE.EncodeJson IconSize where
  encodeJson = AE.encodeJson <<< iconSizeString


iconSizeString :: IconSize -> String
iconSizeString AnyIconSize  = "any"
iconSizeString (Pixels x y) = show x <> "x" <> show y


-------------------------------------------------------------------------------
newtype RelatedApplication = RelatedApplication
  { platform :: String -- probably...
  , url :: URL
  , id :: Maybe String
  }


instance encodeJsonRelatedApplication :: AE.EncodeJson RelatedApplication where
  encodeJson (RelatedApplication o) =
       "platform" := o.platform
    ~> "url" := o.url
    ~> "id" := o.id
    ~> jsonEmptyObject
