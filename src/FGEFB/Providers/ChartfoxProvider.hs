{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module FGEFB.Providers.ChartfoxProvider
where

import Data.List
import Data.Maybe (fromMaybe)
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Network.HTTP.Types (urlDecode)
import Control.Applicative
import qualified Data.Aeson as JSON
import Data.Aeson ( (.:), (.=), (.:?) )
import qualified Data.ByteString.Lazy as LBS

import qualified FGEFB.Chartfox as Chartfox

import FGEFB.Provider
import FGEFB.Util (tshow)
import FGEFB.HTTP (downloadHttp)

getBearerToken :: [(Text, Text)] -> IO Text
getBearerToken query =
  maybe (error "No bearer token provided, log into chartfox first") return $
    lookup "chartfoxToken" query

getSearchQuery :: [(Text, Text)] -> Maybe Text
getSearchQuery query =
  lookup "q" query

topLevelCodes :: Map Text Text
topLevelCodes = Map.fromList
  [ ("AF", "Afghanistan")
  , ("AX", "Åland Islands")
  , ("AL", "Albania")
  , ("DZ", "Algeria")
  , ("AS", "American Samoa")
  , ("AD", "Andorra")
  , ("AO", "Angola")
  , ("AI", "Anguilla")
  , ("AQ", "Antarctica")
  , ("AG", "Antigua and Barbuda")
  , ("AR", "Argentina")
  , ("AM", "Armenia")
  , ("AW", "Aruba")
  , ("AU", "Australia")
  , ("AT", "Austria")
  , ("AZ", "Azerbaijan")
  , ("BS", "Bahamas")
  , ("BH", "Bahrain")
  , ("BD", "Bangladesh")
  , ("BB", "Barbados")
  , ("BY", "Belarus")
  , ("BE", "Belgium")
  , ("BZ", "Belize")
  , ("BJ", "Benin")
  , ("BM", "Bermuda")
  , ("BT", "Bhutan")
  , ("BO", "Bolivia")
  , ("BQ", "Bonaire, Sint Eustatius and Saba")
  , ("BA", "Bosnia and Herzegovina")
  , ("BW", "Botswana")
  , ("BV", "Bouvet Island")
  , ("BR", "Brazil")
  , ("IO", "British Indian Ocean Territory")
  , ("BN", "Brunei Darussalam")
  , ("BG", "Bulgaria")
  , ("BF", "Burkina Faso")
  , ("BI", "Burundi")
  , ("CV", "Cabo Verde")
  , ("KH", "Cambodia")
  , ("CM", "Cameroon")
  , ("CA", "Canada")
  , ("KY", "Cayman Islands")
  , ("CF", "Central African Republic")
  , ("TD", "Chad")
  , ("CL", "Chile")
  , ("CN", "China")
  , ("CX", "Christmas Island")
  , ("CC", "Cocos (Keeling) Islands")
  , ("CO", "Colombia")
  , ("KM", "Comoros")
  , ("CG", "Congo")
  , ("CD", "Congo, Democratic Republic")
  , ("CK", "Cook Islands")
  , ("CR", "Costa Rica")
  , ("CI", "Côte d'Ivoire")
  , ("HR", "Croatia")
  , ("CU", "Cuba")
  , ("CW", "Curaçao")
  , ("CY", "Cyprus")
  , ("CZ", "Czechia")
  , ("DK", "Denmark")
  , ("DJ", "Djibouti")
  , ("DM", "Dominica")
  , ("DO", "Dominican Republic")
  , ("EC", "Ecuador")
  , ("EG", "Egypt")
  , ("SV", "El Salvador")
  , ("GQ", "Equatorial Guinea")
  , ("ER", "Eritrea")
  , ("EE", "Estonia")
  , ("SZ", "Eswatini")
  , ("ET", "Ethiopia")
  , ("FK", "Falkland Islands")
  , ("FO", "Faroe Islands")
  , ("FJ", "Fiji")
  , ("FI", "Finland")
  , ("FR", "France")
  , ("GF", "French Guiana")
  , ("PF", "French Polynesia")
  , ("TF", "French Southern Territories")
  , ("GA", "Gabon")
  , ("GM", "Gambia")
  , ("GE", "Georgia")
  , ("DE", "Germany")
  , ("GH", "Ghana")
  , ("GI", "Gibraltar")
  , ("GR", "Greece")
  , ("GL", "Greenland")
  , ("GD", "Grenada")
  , ("GP", "Guadeloupe")
  , ("GU", "Guam")
  , ("GT", "Guatemala")
  , ("GG", "Guernsey")
  , ("GN", "Guinea")
  , ("GW", "Guinea-Bissau")
  , ("GY", "Guyana")
  , ("HT", "Haiti")
  , ("HM", "Heard Island and McDonald Islands")
  , ("VA", "Holy See")
  , ("HN", "Honduras")
  , ("HK", "Hong Kong")
  , ("HU", "Hungary")
  , ("IS", "Iceland")
  , ("IN", "India")
  , ("ID", "Indonesia")
  , ("IR", "Iran")
  , ("IQ", "Iraq")
  , ("IE", "Ireland")
  , ("IM", "Isle of Man")
  , ("IL", "Israel")
  , ("IT", "Italy")
  , ("JM", "Jamaica")
  , ("JP", "Japan")
  , ("JE", "Jersey")
  , ("JO", "Jordan")
  , ("KZ", "Kazakhstan")
  , ("KE", "Kenya")
  , ("KI", "Kiribati")
  , ("KP", "Korea (Democratic People's Republic)")
  , ("KR", "Korea (Republic)")
  , ("KW", "Kuwait")
  , ("KG", "Kyrgyzstan")
  , ("LA", "Lao")
  , ("LV", "Latvia")
  , ("LB", "Lebanon")
  , ("LS", "Lesotho")
  , ("LR", "Liberia")
  , ("LY", "Libya")
  , ("LI", "Liechtenstein")
  , ("LT", "Lithuania")
  , ("LU", "Luxembourg")
  , ("MO", "Macao")
  , ("MG", "Madagascar")
  , ("MW", "Malawi")
  , ("MY", "Malaysia")
  , ("MV", "Maldives")
  , ("ML", "Mali")
  , ("MT", "Malta")
  , ("MH", "Marshall Islands")
  , ("MQ", "Martinique")
  , ("MR", "Mauritania")
  , ("MU", "Mauritius")
  , ("YT", "Mayotte")
  , ("MX", "Mexico")
  , ("FM", "Micronesia")
  , ("MD", "Moldova, Republic of")
  , ("MC", "Monaco")
  , ("MN", "Mongolia")
  , ("ME", "Montenegro")
  , ("MS", "Montserrat")
  , ("MA", "Morocco")
  , ("MZ", "Mozambique")
  , ("MM", "Myanmar")
  , ("NA", "Namibia")
  , ("NR", "Nauru")
  , ("NP", "Nepal")
  , ("NL", "Netherlands")
  , ("NC", "New Caledonia")
  , ("NZ", "New Zealand")
  , ("NI", "Nicaragua")
  , ("NE", "Niger")
  , ("NG", "Nigeria")
  , ("NU", "Niue")
  , ("NF", "Norfolk Island")
  , ("MK", "North Macedonia")
  , ("MP", "Northern Mariana Islands")
  , ("NO", "Norway")
  , ("OM", "Oman")
  , ("PK", "Pakistan")
  , ("PW", "Palau")
  , ("PS", "Palestine")
  , ("PA", "Panama")
  , ("PG", "Papua New Guinea")
  , ("PY", "Paraguay")
  , ("PE", "Peru")
  , ("PH", "Philippines")
  , ("PN", "Pitcairn")
  , ("PL", "Poland")
  , ("PT", "Portugal")
  , ("PR", "Puerto Rico")
  , ("QA", "Qatar")
  , ("RE", "Réunion")
  , ("RO", "Romania")
  , ("RU", "Russian Federation")
  , ("RW", "Rwanda")
  , ("BL", "Saint Barthélemy")
  , ("SH", "Saint Helena, Ascension and Tristan da Cunha")
  , ("KN", "Saint Kitts and Nevis")
  , ("LC", "Saint Lucia")
  , ("MF", "Saint Martin")
  , ("PM", "Saint Pierre and Miquelon")
  , ("VC", "Saint Vincent and the Grenadines")
  , ("WS", "Samoa")
  , ("SM", "San Marino")
  , ("ST", "Sao Tome and Principe")
  , ("SA", "Saudi Arabia")
  , ("SN", "Senegal")
  , ("RS", "Serbia")
  , ("SC", "Seychelles")
  , ("SL", "Sierra Leone")
  , ("SG", "Singapore")
  , ("SX", "Sint Maarten")
  , ("SK", "Slovakia")
  , ("SI", "Slovenia")
  , ("SB", "Solomon Islands")
  , ("SO", "Somalia")
  , ("ZA", "South Africa")
  , ("GS", "South Georgia and the South Sandwich Islands")
  , ("SS", "South Sudan")
  , ("ES", "Spain")
  , ("LK", "Sri Lanka")
  , ("SD", "Sudan")
  , ("SR", "Suriname")
  , ("SJ", "Svalbard and Jan Mayen")
  , ("SE", "Sweden")
  , ("CH", "Switzerland")
  , ("SY", "Syrian Arab Republic")
  , ("TW", "Taiwan")
  , ("TJ", "Tajikistan")
  , ("TZ", "Tanzania")
  , ("TH", "Thailand")
  , ("TL", "Timor-Leste")
  , ("TG", "Togo")
  , ("TK", "Tokelau")
  , ("TO", "Tonga")
  , ("TT", "Trinidad and Tobago")
  , ("TN", "Tunisia")
  , ("TR", "Turkey")
  , ("TM", "Turkmenistan")
  , ("TC", "Turks and Caicos Islands")
  , ("TV", "Tuvalu")
  , ("UG", "Uganda")
  , ("UA", "Ukraine")
  , ("AE", "United Arab Emirates")
  , ("GB", "United Kingdom")
  , ("US", "USA")
  , ("UM", "USA Minor Outlying Islands")
  , ("UY", "Uruguay")
  , ("UZ", "Uzbekistan")
  , ("VU", "Vanuatu")
  , ("VE", "Venezuela")
  , ("VN", "Viet Nam")
  , ("VG", "Virgin Islands (British)")
  , ("VI", "Virgin Islands (U.S.)")
  , ("WF", "Wallis and Futuna")
  , ("EH", "Western Sahara")
  , ("YE", "Yemen")
  , ("ZM", "Zambia")
  , ("ZW", "Zimbabwe")
  ]

listTopLevel :: Int -> IO FileList
listTopLevel page =
  return . setFileListSearchPath "search" . paginate page . map toFileEntry . sortOn snd . Map.toAscList $ topLevelCodes
  where
    toFileEntry (path, name) =
      defFileInfo
        { fileName = name
        , filePath = path
        , fileType = Directory
        }

data AirportInfo =
  AirportInfo
    { airportInfoID :: Int
    , airportInfoIdent :: Text
    , airportInfoICAO :: Maybe Text
    , airportInfoIATA :: Maybe Text
    , airportInfoName :: Maybe Text
    , airportInfoHasCharts :: Bool
    , airportInfoHasSources :: Bool
    }
    deriving (Show)

data ChartType
  = UnknownChart
  | GeneralChart
  | TextualChart
  | GroundLayoutChart
  | SIDChart
  | STARChart
  | ApproachChart
  | TransitionChart
  | Briefing
  deriving (Show, Ord, Eq, Enum, Bounded)

chartTypeIndex :: ChartType -> Int
chartTypeIndex UnknownChart = 0
chartTypeIndex GeneralChart = 1
chartTypeIndex TextualChart = 2
chartTypeIndex GroundLayoutChart = 3
chartTypeIndex SIDChart = 4
chartTypeIndex STARChart = 5
chartTypeIndex ApproachChart = 6
chartTypeIndex TransitionChart = 7
chartTypeIndex Briefing = 99

chartTypeFromIndex :: Int -> ChartType
chartTypeFromIndex 0 = UnknownChart
chartTypeFromIndex 1 = GeneralChart
chartTypeFromIndex 2 = TextualChart
chartTypeFromIndex 3 = GroundLayoutChart
chartTypeFromIndex 4 = SIDChart
chartTypeFromIndex 5 = STARChart
chartTypeFromIndex 6 = ApproachChart
chartTypeFromIndex 7 = TransitionChart
chartTypeFromIndex 99 = Briefing
chartTypeFromIndex _ = UnknownChart

chartTypeIdent :: ChartType -> Text
chartTypeIdent UnknownChart = "OTHER"
chartTypeIdent GeneralChart = "GEN"
chartTypeIdent TextualChart = "TXT"
chartTypeIdent GroundLayoutChart = "GND"
chartTypeIdent SIDChart = "SID"
chartTypeIdent STARChart = "STAR"
chartTypeIdent ApproachChart = "APP"
chartTypeIdent TransitionChart = "TRANS"
chartTypeIdent Briefing = "BRIEFING"

chartTypeFromIdent :: Text -> ChartType
chartTypeFromIdent "GEN" = GeneralChart
chartTypeFromIdent "TXT" = TextualChart
chartTypeFromIdent "GND" = GroundLayoutChart
chartTypeFromIdent "SID" = SIDChart
chartTypeFromIdent "STAR" = STARChart
chartTypeFromIdent "APP" = ApproachChart
chartTypeFromIdent "TRANS" = TransitionChart
chartTypeFromIdent "BRIEFING" = Briefing
chartTypeFromIdent _ = UnknownChart

chartTypeName :: ChartType -> Text
chartTypeName UnknownChart = "Other"
chartTypeName GeneralChart = "General"
chartTypeName TextualChart = "Text"
chartTypeName GroundLayoutChart = "Ground"
chartTypeName SIDChart = "SID"
chartTypeName STARChart = "STAR"
chartTypeName ApproachChart = "Approach"
chartTypeName TransitionChart = "Transition"
chartTypeName Briefing = "Briefing"

chartFileTypeFromIndex :: Int -> ChartFileType
chartFileTypeFromIndex 0 = ChartFilePDF
chartFileTypeFromIndex 1 = ChartFileIMG
chartFileTypeFromIndex _ = ChartFileUnknown


data ChartInfo =
  ChartInfo
    { chartInfoID :: Text
    , chartInfoParentID :: Maybe Text
    , chartInfoAirportICAO :: Text
    , chartInfoName :: Text
    , chartInfoCode :: Maybe Text
    , chartInfoType :: ChartType
    , chartInfoGeorefs :: [ChartGeoRef]
    }

data ChartDetails =
  ChartDetails
    { chartDetailsID :: Text
    , chartDetailsParentID :: Maybe Text
    , chartDetailsAirportICAO :: Text
    , chartDetailsName :: Text
    , chartDetailsCode :: Maybe Text
    , chartDetailsType :: ChartType
    , chartDetailsURL :: Maybe Text
    , chartDetailsSourceURL :: Maybe Text
    , chartDetailsSourceURLType :: Maybe ChartFileType
    , chartDetailsFiles :: [ChartFileInfo]
    , chartDetailsGeorefs :: [ChartGeoRef]
    }

data ChartGeoRef =
  ChartGeoRef
    { chartGeoTX :: Double
    , chartGeoTY :: Double
    , chartGeoK :: Double
    , chartGeoTransformAngle :: Double
    , chartGeoPdfPageRotation :: Double
    , chartGeoPage :: Int
    }

data ChartFileType
  = ChartFilePDF
  | ChartFileIMG
  | ChartFileUnknown
  deriving (Show, Eq, Ord, Enum, Bounded)

data ChartFileInfo =
  ChartFileInfo
    { chartFileType :: ChartFileType
    , chartFileURL :: Text
    }

data ResponseMeta =
  ResponseMeta
    { metaPath :: Text

    , metaFrom :: Int
    , metaTo :: Int
    , metaTotal :: Int
    , metaPerPage :: Int

    , metaCurrentPage :: Int
    , metaLastPage :: Int

    , metaFirstPageUrl :: Maybe Text
    , metaLastPageUrl :: Maybe Text
    , metaNextPageUrl :: Maybe Text
    , metaPrevPageUrl :: Maybe Text
    }
    deriving (Show)

defResponseMeta :: ResponseMeta
defResponseMeta =
  ResponseMeta
    { metaPath = ""

    , metaFrom = 0
    , metaTo = 0
    , metaTotal = 0
    , metaPerPage = 0

    , metaCurrentPage = 0
    , metaLastPage = 0

    , metaFirstPageUrl = Nothing
    , metaLastPageUrl = Nothing
    , metaNextPageUrl = Nothing
    , metaPrevPageUrl = Nothing
    }

data DataResponse a =
  DataResponse
    { responseData :: a
    , responseMeta :: ResponseMeta
    }
    deriving (Show)

instance JSON.FromJSON AirportInfo where
  parseJSON = JSON.withObject "AirportInfo" $ \obj ->
    AirportInfo
      <$> obj .: "id"
      <*> obj .: "ident"
      <*> obj .: "icao_code"
      <*> obj .: "iata_code"
      <*> obj .: "name"
      <*> obj .: "has_charts"
      <*> obj .: "has_sources"

instance JSON.FromJSON ChartType where
  parseJSON j = chartTypeFromIndex <$> JSON.parseJSON j

instance JSON.FromJSON ChartFileType where
  parseJSON j = chartFileTypeFromIndex <$> JSON.parseJSON j

instance JSON.FromJSON ChartInfo where
  parseJSON = JSON.withObject "ChartInfo" $ \obj ->
    ChartInfo
      <$> obj .: "id"
      <*> obj .: "parent_id"
      <*> obj .: "airport_icao"
      <*> obj .: "name"
      <*> obj .: "code"
      <*> obj .: "type"
      <*> (fromMaybe [] <$> obj .:? "georefs")

instance JSON.FromJSON ChartDetails where
  parseJSON = JSON.withObject "ChartDetails" $ \obj ->
    ChartDetails
      <$> obj .: "id"
      <*> obj .: "parent_id"
      <*> obj .: "airport_icao"
      <*> obj .: "name"
      <*> obj .: "code"
      <*> obj .: "type"
      <*> obj .: "url"
      <*> obj .: "source_url"
      <*> obj .: "source_url_type"
      <*> (fromMaybe [] <$> obj .:? "files")
      <*> (fromMaybe [] <$> obj .:? "georefs")

instance JSON.FromJSON ResponseMeta where
  parseJSON = JSON.withObject "ResponseMeta" $ \obj ->
    ResponseMeta
      <$> obj .: "path"
      <*> obj .: "from"
      <*> obj .: "to"
      <*> obj .: "total"
      <*> obj .: "per_page"
      <*> obj .: "current_page"
      <*> obj .: "last_page"
      <*> obj .: "first_page_url"
      <*> obj .: "last_page_url"
      <*> obj .: "next_page_url"
      <*> obj .: "prev_page_url"

instance JSON.FromJSON ChartFileInfo where
  parseJSON = JSON.withObject "ChartFileInfo" $ \obj ->
    ChartFileInfo
      <$> obj .: "type"
      <*> obj .: "url"

instance JSON.FromJSON ChartGeoRef where
  parseJSON = JSON.withObject "ChartGeoRef" $ \obj ->
    ChartGeoRef
      <$> obj .: "tx"
      <*> obj .: "ty"
      <*> obj .: "k"
      <*> obj .: "transform_angle"
      <*> obj .: "pdf_page_rotation"
      <*> obj .: "page"

instance JSON.FromJSON a => JSON.FromJSON (DataResponse a) where
  parseJSON = JSON.withObject "DataResponse" $ \obj ->
    DataResponse
      <$> obj .: "data"
      <*> (fromMaybe defResponseMeta <$> obj .:? "meta")

fetchPage :: forall a.
             JSON.FromJSON a
          => Text
          -> Text
          -> [(Text, Maybe Text)]
          -> Int
          -> IO (ResponseMeta, [a])
fetchPage bearerToken url query page =
  fetch bearerToken url (("page", Just . tshow $ page) : query)

fetchPaginate :: forall a.
                 JSON.FromJSON a
              => Text
              -> Text
              -> [(Text, Maybe Text)]
              -> Int
              -> IO (FileListMeta, [a])
fetchPaginate bearerToken url query page = do
  (_, items) <- fetch bearerToken url query
  return $ paginateRaw page items

fetch :: forall a.
         JSON.FromJSON a
      => Text
      -> Text
      -> [(Text, Maybe Text)]
      -> IO (ResponseMeta, a)
fetch bearerToken url query = do
    response <- either error return =<< Chartfox.apiCall
                  bearerToken
                  url
                  query
    let items = responseData response
    let meta = responseMeta response
    return (meta, items)

responseMetaToFileListMeta :: ResponseMeta -> FileListMeta
responseMetaToFileListMeta rm =
  nullFileListMeta
    { fileListMetaCurrentPage = metaCurrentPage rm - 1
    , fileListMetaNumPages = metaLastPage rm
    }

listAirports :: Text -> Text -> Int -> IO FileList
listAirports bearerToken countryCode page = do
  (meta, airports) <-
    fetchPage bearerToken url query page
  return FileList
    { fileListFiles =
        map airportToFileInfo . sortAirports $ airports
    , fileListMeta =
        setFileListMetaSearchPath "search" $
          responseMetaToFileListMeta meta
    }

  where
    url = "/v2/airports"
    sortAirports = sortOn airportInfoICAO
    query = [ ("isoCountry", Just countryCode)
            , ("supported", Just "1")
            ]

listSearch :: Text -> Text -> Int -> IO FileList
listSearch bearerToken searchQuery page = do
  (meta, airports) <-
    fetchPage bearerToken url query page
  return FileList
    { fileListFiles = map airportToFileInfo . sortAirports $ airports
    , fileListMeta =
        setFileListMetaSearchPath "search" $
          responseMetaToFileListMeta meta
    }

  where
    url = "/v2/airports"
    sortAirports = sortOn airportInfoICAO
    query = [ ("query", Just searchQuery)
            , ("supported", Just "1")
            ]

chartToFileInfo :: ChartInfo -> FileInfo
chartToFileInfo chart =
  defFileInfo
    { fileName = chartInfoName chart
    , filePath = chartInfoAirportICAO chart <> "/" <> 
                    chartTypeIdent (chartInfoType chart) <> "/" <>
                    chartInfoID chart
    , fileType = PDFFile
    }

chartGeoRefsToFileGeoRefs :: [ChartGeoRef] -> Map Int FileGeoRef
chartGeoRefsToFileGeoRefs [] = mempty
chartGeoRefsToFileGeoRefs (chartGeo:xs) =
  Map.insert page fileGeo (chartGeoRefsToFileGeoRefs xs)
  where
    (page, fileGeo) = chartGeoRefToFileGeoRef chartGeo

chartGeoRefToFileGeoRef :: ChartGeoRef -> (Int, FileGeoRef)
chartGeoRefToFileGeoRef chartGeo =
  ( chartGeoPage chartGeo
  , FileGeoRef
      { fileGeoTX = chartGeoTX chartGeo
      , fileGeoTY = chartGeoTY chartGeo
      , fileGeoK = chartGeoK chartGeo
      , fileGeoTransformAngle = chartGeoTransformAngle chartGeo
      , fileGeoPdfPageRotation = chartGeoPdfPageRotation chartGeo
      }
  )


airportToFileInfo :: AirportInfo -> FileInfo
airportToFileInfo airport =
  defFileInfo
    { fileName = fromMaybe "???" $ do
        ident <- airportInfoICAO airport
        name <- airportInfoName airport
        return $ ident <> " " <> name
    , filePath = airportInfoIdent airport
    , fileType = Directory
    }

listAirport :: Text -> Text -> Int -> IO FileList
listAirport bearerToken icao page = do
  (_, chartsGrouped :: Map Int [ChartInfo]) <- fetch bearerToken url []
  return FileList
    { fileListFiles =
        [ defFileInfo
            { fileName = icao <> " " <> chartTypeName t
            , filePath = icao <> "/" <> chartTypeIdent t
            , fileType = Directory
            }
        | i <- Map.keys chartsGrouped
        , let t = chartTypeFromIndex i
        ]
    , fileListMeta = nullFileListMeta
    }
  where
    sortCharts = sortOn (\c -> (chartInfoType c, chartInfoCode c, chartInfoName c))
    url = "/v2/airports/" <> icao <> "/charts/grouped"

listAirportCharts :: Text -> Text -> ChartType -> Int -> IO FileList
listAirportCharts bearerToken icao chartType page = do
  (_, chartsGrouped) <- fetch bearerToken url []
  let chartsRaw = fromMaybe [] $ Map.lookup (chartTypeIndex chartType) chartsGrouped
  let (meta, charts) = paginateRaw page chartsRaw
  return FileList
    { fileListFiles = map chartToFileInfo . sortCharts  $ charts
    , fileListMeta = meta
    }
  where
    sortCharts = sortOn (\c -> (chartInfoType c, chartInfoCode c, chartInfoName c))
    url = "/v2/airports/" <> icao <> "/charts/grouped"

downloadChart :: Text -> Text -> IO (Maybe FileDetails)
downloadChart bearerToken chartID = do
  infoEither <- Chartfox.apiCall bearerToken ("/v2/charts/" <> chartID) []
  case infoEither of
    Right info -> runMaybeT $ do
      path <- case (filter isPdf $ chartDetailsFiles info) of
        ChartFileInfo { chartFileURL = url }:_ -> do
          liftIO $ print url
          liftIO $ downloadHttp url ".pdf"
        [] -> do
          url <- MaybeT $ pure (chartDetailsSourceURL info)
          ty <- MaybeT $ pure (chartDetailsSourceURLType info)
          if ty == ChartFilePDF then do
            liftIO $ print url
            liftIO $ downloadHttp url ".pdf"
          else
            fail "No chart"
      return $ FileDetails
        { fileDetailsPath = path
        , fileDetailsGeorefs = chartGeoRefsToFileGeoRefs (chartDetailsGeorefs info)
        }
    Left err -> do
      print err
      return Nothing
  where
    isPdf f = chartFileType f == ChartFilePDF

chartfoxProvider :: Maybe Text -> Provider
chartfoxProvider labelMay = Provider
  { label = labelMay <|> (Just "ChartFox")
  , listFiles = \query path page -> do
      bearerToken <- getBearerToken query
      let pathParts = Text.splitOn "/" path
      print bearerToken
      print pathParts
      case pathParts of
        [] ->
          listTopLevel page
        [""] ->
          listTopLevel page
        ["search"] -> do
          print "Search"
          case getSearchQuery query of
            Nothing -> do
              print "No search query"
              return $ setFileListSearchPath "search" nullFileList
            Just searchQuery -> do
              print searchQuery
              listSearch bearerToken searchQuery page
        [item] | Text.length item < 4 ->
          listAirports bearerToken item page
        [item] ->
          listAirport bearerToken item page
        [item, tyIdent] | ty <- chartTypeFromIdent tyIdent ->
          listAirportCharts bearerToken item ty page
        x -> do
          print x
          return $ FileList [] nullFileListMeta
  , getPdf = \query path -> do
      bearerToken <- getBearerToken query
      let pathParts = Text.splitOn "/" path
      print pathParts
      case pathParts of
        [_icao, _type, chartID] ->
          downloadChart bearerToken chartID
        [_icao, chartID] ->
          downloadChart bearerToken chartID
        _ ->
          error "Invalid chart"
  }
