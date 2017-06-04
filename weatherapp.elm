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
      (Model zipCode "" "" "" model.weatherData 
                              model.resultCount, Cmd.none)

    NewWeatherData (Ok newWeatherData) ->
      (Model model.zipCode "" "" "" newWeatherData
             model.resultCount, Cmd.none)
           
    NewWeatherData (Err msg) ->
      (Model model.zipCode "" "" "" ("Error " ++ toString msg) 0, Cmd.none)
{--
    NewWeatherData (Ok newWeatherData) ->
      (Model model.zipCode "" "" "" newWeatherData 0, SetResultCount)
        |> andThen decodeResultCount --(model, Cmd.none)
--}

{--
    NewWeatherData (Ok newWeatherData) ->
      case decodeResultCount model.weatherData of
        Err msg ->
          (Model model.zipCode "" "" "" (newWeatherData ++ msg) 0, Cmd.none)
           
        Ok value ->
          (Model model.zipCode "" "" "" newWeatherData value, Cmd.none)

    NewWeatherData (Err msg) ->
      (Model model.zipCode "" "" "" ("Error " ++ toString msg) 0, Cmd.none)
--}    

{--
    SetResultCount ->
      (Model model.zipCode "" "" "" model.weatherData 
             (String.toInt (decodeResultCount model)), Cmd.none)
--}


-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ h2 []  [ text model.zipCode ]
    , input  [ type_ "text", placeholder "Enter Zip Code", 
                             onInput SetZipCode ] []
    , button [ onClick GetWeather ] [ text "Get Weather" ]
    , br []  []
    , p  []  [ text model.weatherData ]
    , updateResultCount model
    ]


updateResultCount : Model -> Html Msg
updateResultCount model =
  let count = 
    decodeResultCount model.weatherData
  in
    case count of
      Ok value ->
        h3 [] [ text ("Results: " ++ toString value) ]

      Err msg ->
        h3 [] [ text "No Results" ]


decodeResultCount : String -> Result String Int
decodeResultCount json = decodeString (field "metadata" 
                         (field "resultset" (field "count" int))) json


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
    , timeout = Nothing
    , withCredentials = False
    }
  in
    Http.send NewWeatherData (Http.request weatherRequest)
