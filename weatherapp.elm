import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (..)


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
  , dataSetId   : String
  , submit      : Bool
  }

type alias GhcndData = 
  { date        : String
  , datatype    : String
  , station     : String
  , attributes  : String
  , value       : Int
  }


ghcndData : Decoder GhcndData
ghcndData =
  map5 GhcndData
    (at ["date"]        string)
    (at ["datatype"]    string)
    (at ["station"]     string)
    (at ["attributes"]  string)
    (at ["value"]       int)


init : (Model, Cmd Msg)
init =
  (Model "" 
         "" 
         ""
         ""
         "" 
         "GHCND" 
         False, Cmd.none)



-- UPDATE


type Msg
  = GetWeather
  | NewWeatherData (Result Http.Error String)
  | SetZipCode    String
  | SetStationId  String
  | SetStartDate  String
  | SetEndDate    String
  | SetDataSetId  String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GetWeather ->
      (Model model.zipCode
             model.stationId
             model.startDate
             model.endDate
             model.weatherData
             model.dataSetId
             True
           , getWeather model)

    NewWeatherData (Ok newWeatherData) ->
      (Model model.zipCode 
             model.stationId 
             model.startDate 
             model.endDate 
             newWeatherData 
             model.dataSetId
             model.submit
           , Cmd.none)
           
    NewWeatherData (Err msg) ->
      (Model model.zipCode 
             model.stationId 
             model.startDate 
             model.endDate 
             ("Error " ++ toString msg)
             model.dataSetId
             model.submit
           , Cmd.none)

    SetZipCode zipCode ->
      (Model zipCode 
             model.stationId 
             model.startDate 
             model.endDate 
             model.weatherData 
             model.dataSetId 
             model.submit
           , Cmd.none)

    SetStationId stationId ->
      (Model model.zipCode 
             stationId 
             model.startDate 
             model.endDate 
             model.weatherData 
             model.dataSetId
             model.submit
           , Cmd.none)

    SetStartDate startDate ->
      (Model model.zipCode 
             model.stationId 
             startDate 
             model.endDate 
             model.weatherData 
             model.dataSetId
             model.submit
           , Cmd.none)

    SetEndDate endDate ->
      (Model model.zipCode 
             model.stationId 
             model.startDate 
             endDate 
             model.weatherData 
             model.dataSetId
             model.submit
           , Cmd.none)

    SetDataSetId dataSetId ->
      (Model model.zipCode 
             model.stationId 
             model.startDate 
             model.endDate 
             model.weatherData 
             dataSetId
             model.submit
           , Cmd.none)

 

-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ h2     []  [ text "NOAA Weather Data Query" ]

    , input  [ type_ "text", placeholder "Enter Zip Code", 
                             onInput SetZipCode ] []

    , input  [ type_ "text", placeholder "Start Date (yyyy-mm-dd)" 
                           , onInput SetStartDate ] []

    , input  [ type_ "text", placeholder "End Date (yyyy-mm-dd)" 
                           , onInput SetEndDate ] []

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


updateWeatherData : Model -> Html Msg
updateWeatherData model =
  let data =
    decodeString (field "results" 
                   (Json.Decode.list ghcndData)) model.weatherData 
  in
     case data of
       Ok listOfValues ->
         makeGhcndTable listOfValues

       Err msg ->
         p [] [ text ("Couldn't decode weather data: " ++ msg) ]


makeGhcndTable : List GhcndData -> Html Msg
makeGhcndTable listOfValues =
  table [] [ makeGhcndTableHeader, makeGhcndTableBody listOfValues ]


makeGhcndTableBody : List GhcndData -> Html Msg
makeGhcndTableBody listOfValues = 
  listOfValues
    |> List.map (\a -> (makeGhcndTableRow a))
    |> tbody []
  

makeGhcndTableRow : GhcndData -> Html Msg
makeGhcndTableRow data = 
  tr [] [ 
     td [] [ text (toString data.date) ]
   , td [] [ text (toString data.datatype) ]
   , td [] [ text (toString data.station) ]
   , td [] [ text (toString data.attributes) ]
   , td [] [ text (toString data.value) ]
  ] 


makeGhcndTableHeader : Html Msg
makeGhcndTableHeader = 
  tr [] [
     td [] [ text "Date" ]
   , td [] [ text "Data Type" ]
   , td [] [ text "Station" ]
   , td [] [ text "Attributes" ]
   , td [] [ text "Value" ]
  ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- HTTP


--getWeatherDataTypes : Cmd Msg

getWeather : Model -> Cmd Msg
getWeather model = 
  if model.submit == True then
    let weatherRequest = 
      { method  = "GET"
      , headers = [Http.header "token" "hntEzDOlCPVILyHVyUIAzvQkvPbrkEBG"]
      , url     = "https://www.ncdc.noaa.gov/cdo-web/api/v2/data?"
                ++"datasetid="      ++ model.dataSetId ++ "&"
                ++"locationid=ZIP:" ++ model.zipCode   ++ "&"
                ++"startdate="      ++ model.startDate ++ "&"
                ++"enddate="        ++ model.endDate
      , body            = Http.emptyBody
      , expect          = Http.expectString
      , timeout         = Nothing
      , withCredentials = False
      }
    in
      Http.send NewWeatherData (Http.request weatherRequest)
  else
    Cmd.none
