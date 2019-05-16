module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (decode, optional)


type Msg
    = FillZipCode String
    | ReceiveAddress (Result Http.Error Address)


type alias Address =
    { cep : String
    , logradouro : String
    , bairro : String
    , localidade : String
    , uf : String
    }


type alias Model =
    { address : Address
    , message : Maybe String
    }

showMessage model = 
    model.message

init : ( Model, Cmd msg )
init =
    ( { address = Address "" "" "" "" "", message = Nothing }
    , Cmd.none
    )


url : String -> String
url cep =
    "http://viacep.com.br/ws/" ++ cep ++ "/json/"


httpCommand : String -> Cmd Msg
httpCommand zipcode =
    addressDecoder
    |> Http.get (url zipcode)
    |> Http.send ReceiveAddress


show model  = model.message 


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FillZipCode zipcode ->
            if String.length zipcode == 8 then
                ( { model | message = Just "buscando cep" }, httpCommand zipcode )
            else
                ( { model | message = Nothing }, Cmd.none )

        ReceiveAddress (Ok addressFound) ->
            ( { model | message = Nothing, address = addressFound }, Cmd.none )

        ReceiveAddress (Err httpError) ->
            ( { model | message = Just "Erro ao buscar endereço" }, Cmd.none )


addressDecoder : Decoder Address
addressDecoder =
    decode Address
    |> optional "cep" string ""
    |> optional "logradouro" string ""
    |> optional "bairro" string ""
    |> optional "localidade" string ""
    |> optional "uf" string ""


view : Model -> Html Msg
view model =
    div
    [ style [ ( "margin", "20px" ) ] ]
    [ divMessage model
    , input
    [ type_ "text", placeholder "cep", onInput FillZipCode ]
    []
    , inputModel model.address.logradouro "rua" 300
    , inputModel model.address.bairro "bairro" 200
    , inputModel model.address.localidade "cidade" 200
    , inputModel model.address.uf "uf" 200
    ]


inputModel : String -> String -> Int -> Html Msg
inputModel campo label width =
    input
    [ type_ "text"
    , style [ ( "width", toString width ++ "px" ) ]
    , value campo
    , placeholder label
    ]
    []


divMessage : Model -> Html Msg
divMessage model =
    div
    [ style
    [ ( "color", "red" )
    , ( "display", toggleMessage model )
    ]
    ]
    [ text (Maybe.withDefault "" model.message) ]


toggleMessage : Model -> String
toggleMessage model =
    if model.message == Nothing then
        "none"
    else
        "block"


main : Program Never Model Msg
main =
    Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }
