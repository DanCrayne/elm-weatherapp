import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (..)
--import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)



main =
  Html.program
    { init = init 
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model =
  { zipCode     : String
  , stationId   : String
  , startDate   : String
  , endDate     : String
  , weatherData : String
  , resultCount : Int
  , dataSetId   : String
  , ghcndData   : List GhcndData
  }

type alias GhcndData = 
  { date        : String
  , datatype    : String
  , station     : String
  , attributes  : String
  , value       : Int
  }

init : (Model, Cmd Msg)
init =
  (Model "" "" "" "" "" 0 "GHCND" [], Cmd.none)



-- UPDATE


type Msg
  = GetWeather
  | NewWeatherData (Result Http.Error String)
  | SetZipCode String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GetWeather ->
      (model, getWeather model)

    SetZipCode zipCode ->
      (Model zipCode "" "" "" model.weatherData 
                              model.resultCount
                              model.dataSetId [], Cmd.none)

    NewWeatherData (Ok newWeatherData) ->
      (Model model.zipCode "" "" "" newWeatherData
                                    model.resultCount
                                    model.dataSetId [], Cmd.none)
           
    NewWeatherData (Err msg) ->
      (Model model.zipCode "" "" "" ("Error " ++ toString msg) 
                                    0 
                                    model.dataSetId [], Cmd.none)



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ h2 []  [ text "NOAA Weather Data Query" ]
    , input  [ type_ "text", placeholder "Enter Zip Code", 
                             onInput SetZipCode ] []
    , button [ onClick GetWeather ] [ text "Get Weather" ]
    , updateResultCount model
    , updateWeatherData model
    ]


updateResultCount : Model -> Html Msg
updateResultCount model =
  let count = 
    decodeResultCount model.weatherData
  in
    case count of
      Ok value ->
        h3 [] [ text ((toString value) ++ " results for zipcode " 
                                       ++ model.zipCode) ]

      Err msg ->
        h3 [] [ text "No Results" ]




decodeResultCount : String -> Result String Int
decodeResultCount json = 
  decodeString (field "metadata" 
                 (field "resultset" 
                   (field "count" int))) json


{- 
updateWeatherData : Model -> Html Msg
updateWeatherData model = p [] [ text model.weatherData ]
-}

updateWeatherData : Model -> Html Msg
updateWeatherData model =
  let data =
    decodeString (field "results" 
                   (Json.Decode.list ghcndData)) model.weatherData 
--    decodeWeatherData model.weatherData
  in
     case data of
       Ok listOfValues ->
         let firstVal = 
           List.head listOfValues

         in
            case firstVal of
              Just firstVal ->
                div [] [ 
                         p [] [ text (("Date: " ++ (toString firstVal.date)) ++
                              ("\nDatatype: " ++ (toString firstVal.datatype)) ++
                              ("\nStation: " ++ (toString firstVal.station)) ++
                              ("\nAttributes: " ++ (toString firstVal.attributes)) ++
                              ("\nValue: " ++(toString firstVal.value)))
                              ] 
                        ]


                

              Nothing ->
                p [] [ text "Couldn't get first value." ]

       Err msg ->
         p [] [ text ("Couldn't decode weather data: " ++ msg) ]

{-
     case data of
       Ok listOfValues ->
         div [] [ p [] [ text (toString (List.head listOfValues)) ] ]

       Err msg ->
         div [] [ p [] [ text ("Couldn't decode weather data: " ++ msg) ] ] 
         -}


decodeWeatherData : String -> Result String (List GhcndData)
decodeWeatherData weatherData = 
  decodeString (Json.Decode.list ghcndData) weatherData


ghcndData : Decoder GhcndData
ghcndData =
  map5 GhcndData
    (at ["date"]        string)
    (at ["datatype"]    string)
    (at ["station"]     string)
    (at ["attributes"]  string)
    (at ["value"]       int)

--ghcndData = map5 GhcndData (at ["date"]        string) (at ["datatype"]    string) (at ["station"]     string) (at ["attributes"]  string) (at ["value"]       int)
{-
ghcndDataDecoder : Decoder GhcndData
ghcndDataDecoder =
  decode GhcndData
    |> Json.Decode.Pipeline.required "date"       string
    |> Json.Decode.Pipeline.required "datatype"   string
    |> Json.Decode.Pipeline.required "station"    string
    |> Json.Decode.Pipeline.optional "attributes" string "none"
    |> Json.Decode.Pipeline.required "value"      int
-}

--decodeWeatherData : Model -> Result String (List String)
--decodeWeatherData model = decodeString (field "results" string)
--                                         model.weatherData

{--
decodeWeatherData : Model -> Result String (List GhcndData)
decodeWeatherData model = decodeString (field "results" 
                                         (Json.Decode.list GhcndData)) 
                                         model.weatherData
--}
--decodeGhcndData : String ->



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- HTTP


--getWeatherDataTypes : Cmd Msg

getWeather : Model -> Cmd Msg
getWeather model = 
  let weatherRequest = 
    { method = "GET"
    , headers = [Http.header "token" "hntEzDOlCPVILyHVyUIAzvQkvPbrkEBG"]
    , url = "https://www.ncdc.noaa.gov/cdo-web/api/v2/data?"
            ++"datasetid=" ++ model.dataSetId ++ "&"
            ++"locationid=ZIP:" ++ model.zipCode ++ "&"
            ++"startdate=2010-05-01&"
            ++"enddate=2010-05-01"
    , body = Http.emptyBody
    , expect = Http.expectString
    , timeout = Nothing
    , withCredentials = False
    }
  in
    Http.send NewWeatherData (Http.request weatherRequest)
