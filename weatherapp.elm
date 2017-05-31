import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode



main =
  Html.program
    { init = init "97045"
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
      (Model model.zipCode "" "" "" newWeatherData, Cmd.none)
--      (Model model.weatherData newWeatherData, Cmd.none)

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
    , headers = [Http.header "token" "<some token>"]
    , url = "https://www.ncdc.noaa.gov/cdo-web/api/v2/data?datasetid=GHCND&locationid=ZIP:28801&startdate=2010-05-01&enddate=2010-05-01"
    , body = Http.emptyBody
    , expect = Http.expectString
--    , expect = expectStringResponse (\_ -> Ok ())
    , timeout = Nothing
    , withCredentials = False
    }
  in
    Http.send NewWeatherData (Http.request weatherRequest)
