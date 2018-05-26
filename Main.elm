module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, decodeString, field, map2, string)


type Msg
    = MostraEndereco
    | PreencherCep String
    | PreencherLogra String
    | BuscarCep (Result Http.Error Endereco)


type alias Endereco =
    { cep : String
    , logradouro : String
    }


type alias Model =
    { endereco : Endereco
    , mensagem : String
    }


init : ( Model, Cmd msg )
init =
    ( { endereco = Endereco "" "", mensagem = "" }
    , Cmd.none
    )


url : String -> String
url cep =
    "http://viacep.com.br/ws/" ++ cep ++ "/json/"


httpCommand : String -> Cmd Msg
httpCommand cep =
    cepDecoder
        |> Http.get (url cep)
        |> Http.send BuscarCep


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MostraEndereco ->
            ( model, Cmd.none )

        PreencherCep valor ->
            if String.length valor == 8 then
                ( { model | mensagem = "buscando cep" }, httpCommand valor )
            else
                ( { model | endereco = Endereco valor model.endereco.logradouro }, Cmd.none )

        PreencherLogra valor ->
            ( { model | endereco = Endereco model.endereco.cep valor }, Cmd.none )

        BuscarCep (Ok enderecoEncontrado) ->
            ( { model | mensagem = toString enderecoEncontrado }, Cmd.none )

        BuscarCep (Err httpError) ->
            ( model, Cmd.none )


cepDecoder : Decoder Endereco
cepDecoder =
    map2 Endereco
        (field "cep" string)
        (field "logradouro" string)


view : Model -> Html Msg
view model =
    div
        [ style [ ( "margin", "20px" ) ] ]
        [ div [ style [ ( "color", "red" ) ] ]
            [ text (toString <| model.mensagem) ]
        , input
            [ type_ "text", placeholder "cep", onInput PreencherCep ]
            []
        , input [ type_ "text", placeholder "rua", onInput PreencherLogra ] []
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
