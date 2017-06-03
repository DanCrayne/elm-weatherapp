import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (..)



main =
  Html.program
    { init = init "97202"
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
  }


init : String -> (Model, Cmd Msg)
init zipCode =
  (Model zipCode "" "" "" "", Cmd.none)



-- UPDATE


type Msg
  = GetWeather
  | NewWeatherData (Result Http.Error String)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GetWeather ->
      (model, getWeather model.zipCode)

    NewWeatherData (Ok newWeatherData) ->
      case decodeJson of
        Err msg ->
          (Model model.zipCode "" "" "" (newWeatherData ++ msg), Cmd.none)
           
        Ok value ->
          (Model model.zipCode "" "" "" (newWeatherData ++ toString value), Cmd.none)

--      (Model model.zipCode "" "" "" newWeatherData, Cmd.none)

    NewWeatherData (Err _) ->
      (Model model.zipCode "" "" "" "Error while sending", Cmd.none)



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ h2 []  [text model.zipCode]
    , button [ onClick GetWeather ] [ text "Get Weather" ]
    , br []  []
    , p  []  [ text model.weatherData ]
    ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- HTTP


getWeather : String -> Cmd Msg
getWeather zipCode = 
  let weatherRequest = 
    { method = "GET"
    , headers = [Http.header "token" "hntEzDOlCPVILyHVyUIAzvQkvPbrkEBG"]
    , url = "https://www.ncdc.noaa.gov/cdo-web/api/v2/data?datasetid=GHCND&locationid=ZIP:28801&startdate=2010-05-01&enddate=2010-05-01"
    , body = Http.emptyBody
    , expect = Http.expectString
--    , expect = expectStringResponse (\_ -> Ok ())
    , timeout = Nothing
    , withCredentials = False
    }
  in
    Http.send NewWeatherData (Http.request weatherRequest)


decodeJson : Result String Int
decodeJson = decodeString (field "metadata" (field "resultset" (field "offset" int))) json


json : String
json = """
{
  "metadata": {
    "resultset": {
      "offset": 1,
      "count": 8,
      "limit": 25
    }
  },

  "results": [
    {
      "date": "2010-05-01T00:00:00",
      "datatype": "PRCP",
      "station": "GHCND:US1NCBC0005",
      "attributes": ",,N,",
      "value": 0
    },
    {
      "date": "2010-05-01T00:00:00",
      "datatype": "SNOW",
      "station": "GHCND:US1NCBC0005",
      "attributes": ",,N,",
      "value": 0
    },
    {
      "date": "2010-05-01T00:00:00",
      "datatype": "PRCP",
      "station": "GHCND:USW00013872",
      "attributes": ",,0,2400",
      "value": 3
    },
    {
      "date": "2010-05-01T00:00:00",
      "datatype": "SNOW",
      "station": "GHCND:USW00013872",
      "attributes": ",,0,",
      "value": 0
    },
    {
      "date": "2010-05-01T00:00:00",
      "datatype": "SNWD",
      "station": "GHCND:USW00013872",
      "attributes": ",,0,",
      "value": 0
    },
    {
      "date": "2010-05-01T00:00:00",
      "datatype": "TMAX",
      "station": "GHCND:USW00013872",
      "attributes": ",,0,2400",
      "value": 267
    },
    {
      "date": "2010-05-01T00:00:00",
      "datatype": "TMIN",
      "station": "GHCND:USW00013872",
      "attributes": ",,0,2400",
      "value": 139
    },
    {
      "date": "2010-05-01T00:00:00",
      "datatype": "TOBS",
      "station": "GHCND:USW00013872",
      "attributes": ",,0,2400",
      "value": 206
    }
  ]
}
"""
