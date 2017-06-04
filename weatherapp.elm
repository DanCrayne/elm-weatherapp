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
  , resultCount : Int
  }


init : (Model, Cmd Msg)
init =
  (Model "" "" "" "" "" 0, Cmd.none)



-- UPDATE


type Msg
  = GetWeather
  | NewWeatherData (Result Http.Error String)
  | SetZipCode String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GetWeather ->
      (model, getWeather model.zipCode)

    SetZipCode zipCode ->
      (Model zipCode "" "" "" model.weatherData 0, Cmd.none)

    NewWeatherData (Ok newWeatherData) ->
      case decodeResultCount of
        Err msg ->
          (Model model.zipCode "" "" "" (newWeatherData ++ msg) 0, Cmd.none)
           
        Ok value ->
          (Model model.zipCode "" "" "" newWeatherData value, Cmd.none)

    NewWeatherData (Err _) ->
      (Model model.zipCode "" "" "" "Error while sending" 0, Cmd.none)



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ h2 []  [text model.zipCode]
    , input [ type_ "text", placeholder "Enter Zip Code", onInput SetZipCode ] []
    , button [ onClick GetWeather ] [ text "Get Weather" ]
    , br []  []
    , p  []  [ text model.weatherData ]
    , h3 []  [ text ("Result Count: " ++ toString model.resultCount) ]
    ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- HTTP


--getWeatherDataTypes : Cmd Msg

getWeather : String -> Cmd Msg
getWeather zipCode = 
  let weatherRequest = 
    { method = "GET"
    , headers = [Http.header "token" "hntEzDOlCPVILyHVyUIAzvQkvPbrkEBG"]
    , url = "https://www.ncdc.noaa.gov/cdo-web/api/v2/data?datasetid=GHCND&locationid=ZIP:" ++ zipCode ++ "&startdate=2010-05-01&enddate=2010-05-01"
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

--decodeResultCount : Model -> Result String Int
--decodeResultCount model = decodeString (field "metadata" (field "resultset" (field "count" int))) model.weatherData

decodeResultCount : Result String Int
decodeResultCount = decodeString (field "metadata" (field "resultset" (field "count" int))) json

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
