import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (style)
import List exposing (..)
import Http exposing (..)
import Json.Decode exposing (Decoder, int, string, list, nullable)
import Json.Decode.Pipeline exposing (decode, required, hardcoded, optional)
import String exposing (slice)
import Dict exposing (..)


-- MODEL


type alias Model =
    { games: List Game
    , newGameUrl: Maybe String
    , notification: String
    , gameView: Maybe GameView
    }

init : ( Model, Cmd Msg )
init =
    let
        model = { games = [], newGameUrl = Nothing, notification = "", gameView = Nothing  }
    in
        (model , getGames)

type alias Game = 
  { id: String
  , status: String
  , self: String
  , join: Maybe String
  }

type alias NewGameResponse = 
    { url: String
    , playerId: String
    }

type alias GamesResponse =
    { games: List Game
    , newGameUrl: Maybe String
    , notification: String
    }

type alias GameView = 
    { id: String 
    , status: String
    , grid: List (List String)
    , playerId: Maybe String
    , playUrl: String
    }

type Player = X | O

type alias MyGames = Dict String (String, Player)

-- MESSAGES


type Msg
    = Games
    | OnGames (Result Http.Error GamesResponse)
    | NewGame
    | OnNewGame (Result Http.Error NewGameResponse)
    | View (String, Maybe String)
    | OnView (Result Http.Error GameView)
    | Join String


-- VIEW


toTableRow: Game -> Html Msg
toTableRow game =
  tr []
     [ td[] [text (slice 0 6 game.id)]
     , td[] [text game.status]
     , td[] [ button [ onClick (View (game.self, Nothing))] [ text "view" ] 
            , game.join |> Maybe.map (\url -> button [ onClick (Join url)] [ text "join" ] ) |> Maybe.withDefault (text "") ]
     ]

toGridTable : List (List String) -> Html Msg
toGridTable grid =
    table [ style [ ("border", "1px solid black"), ("border-collapse", "collapse") ] ]
          (List.map (\x ->
                tr []
                    (List.map (\y ->                        
                        td[ style [ ("border", "1px solid black"), ("width", "30px"), ("height", "30px"), ("text-align", "center")]] [text y]
                    ) x)
            ) grid )
          

toGameView: GameView -> Html Msg
toGameView gameView =
    div [] 
        [ toGridTable gameView.grid
        , div [] [ p [] [ text ("game: " ++ gameView.id) ] ]
        , div [] [ p [] [  text ("status: " ++ gameView.status) ]]
        ]

view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "Tic Tac Toe" ]
        , model.gameView |> Maybe.map toGameView |> Maybe.withDefault (div [] [])
        , div [] [ text model.notification ]        
        , button [ onClick Games ] [ text "refresh" ] 
        , button [ onClick NewGame ] [ text "new game" ] 
        , table []            
            (List.concat 
                [
                    [ thead []
                        [ th [] [ text "game" ]
                        , th [] [ text "status" ]
                        , th [] [ text "actions" ]
                        ]             
                    ]
                , List.map toTableRow model.games
                ])
        ]


-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Games ->
            ( model, getGames )


        OnGames (Ok response) -> 
            ( { model | games = response.games
              , notification = response.notification
              , newGameUrl = response.newGameUrl
              } 
            , Cmd.none)


        OnGames (Err err) ->
            ( { model | notification = (toString err) } , Cmd.none)


        OnNewGame (Ok response) -> 
            ( model , getGame response.url (Just response.playerId))


        OnNewGame (Err err) ->
            ( { model | notification = (toString err) } , Cmd.none)


        NewGame ->
          case model.newGameUrl of
              Just url -> ( model, postNewGame url)
              Nothing  -> ( model, Cmd.none)
              
        View (url, playerId) ->
            ( model, getGame url playerId)


        OnView (Ok response) ->
            ( { model | gameView = Just response }, Cmd.none)


        OnView (Err err) ->
            (model, Cmd.none)            


        Join url ->
            ( { model | notification = "join game (url = " ++ url ++ ")" }, Cmd.none)            

-- JSON


linkDecoder : String -> Decoder String
linkDecoder rel = Json.Decode.at [ rel, "href" ] string

maybeLinkDecoder : String -> Decoder (Maybe String)
maybeLinkDecoder rel =
    Json.Decode.maybe (Json.Decode.at [ rel, "href" ] string)

gameDecoder : Decoder Game
gameDecoder =
    decode Game
        |> required "id" string
        |> required "status" string
        |> required "_links" (linkDecoder "self")
        |> optional "_links" (maybeLinkDecoder "http://secret-badlands-62551.herokuapp.com/docs/rels/join") Nothing

gamesDecoder : Decoder (List Game)
gamesDecoder =
    Json.Decode.at [ "http://secret-badlands-62551.herokuapp.com/docs/rels/games" ] (list gameDecoder)


gamesResponseDecoder : Decoder GamesResponse
gamesResponseDecoder =
    decode GamesResponse
        |> required "_embedded" gamesDecoder
        |> optional "_links" (maybeLinkDecoder "http://secret-badlands-62551.herokuapp.com/docs/rels/newgame") Nothing
        |> hardcoded ""


newGameDecoder : Decoder String
newGameDecoder = Json.Decode.at [ "playerId" ] string


gameViewDecoder : Maybe String -> Decoder GameView
gameViewDecoder maybePlayerId =
    decode GameView
        |> required "id" string
        |> required "status" string
        |> required "grid" (list (list string))
        |> hardcoded maybePlayerId
        |> required "_links" (linkDecoder "http://secret-badlands-62551.herokuapp.com/docs/rels/play")

-- HTTP

-- GET


getGames : Cmd Msg
getGames  =
    gamesResponseDecoder
    |> Http.get "http://secret-badlands-62551.herokuapp.com/api/games" 
    |> Http.send OnGames

getGame : String -> Maybe String -> Cmd Msg
getGame url maybePlayerId =
    gameViewDecoder maybePlayerId
    |> Http.get url
    |> Http.send OnView    


-- POST

mkPostRequest : Expect a -> String -> Body -> Request a
mkPostRequest exp url body =
  request
    { method = "POST"
    , headers = []
    , url = url
    , body = body
    , expect = exp
    , timeout = Nothing
    , withCredentials = False
    }

postNewGame : String -> Cmd Msg
postNewGame url =
    mkPostRequest 
    (expectStringResponse (\response -> 
        let 
            urlResult = Dict.get "Location" response.headers |> Result.fromMaybe "could not find Location header"
            playerId = Json.Decode.decodeString newGameDecoder response.body
        in
            Result.map2 NewGameResponse urlResult playerId))
    url
    emptyBody
    |> Http.send OnNewGame

-- MAIN


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = (always Sub.none)
        }