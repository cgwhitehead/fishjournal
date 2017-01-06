port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (href, class, style, placeholder, type_)
import Material
import Material.Scheme
import Material.Button as Button
import Material.Textfield as Textfield
import Material.List as Lists


--import Material.Options exposing (css)


main : Program (List Fishes) Model Msg
main =
    Html.programWithFlags
        { init = init, view = view, update = updateWithStorage, subscriptions = \_ -> Sub.none }


port setStorage : List Fishes -> Cmd msg


updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let
        ( newModel, cmds ) =
            update msg model
    in
        ( newModel, Cmd.batch [ setStorage newModel.fishObject, cmds ] )



-- MODEL


type alias Model =
    { fishObject : List Fishes
    , caughtBy : String
    , fishLocation : String
    , fishBait : String
    , isVisible : Bool
    , mdl :
        Material.Model
    }


type alias Fishes =
    { fishId : Int
    , whoCaught : String
    , location : String
    , bait : String
    }


emptyModel : Model
emptyModel =
    { fishObject = []
    , caughtBy = ""
    , fishLocation = ""
    , fishBait = ""
    , isVisible = False
    , mdl =
        Material.model
    }


init : List Fishes -> ( Model, Cmd Msg )
init savedModel =
    let
        newModel =
            { emptyModel | fishObject = Maybe.withDefault [] savedModel }
    in
        newModel ! []



-- Update


type Msg
    = Save
    | Caughtby String
    | Bait String
    | Location String
    | Changevis
    | Mdl (Material.Msg Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Caughtby caughtBy ->
            ({ model | caughtBy = caughtBy }
                ! []
            )

        Bait fishBait ->
            ({ model | fishBait = fishBait }
                ! []
            )

        Location fishLocation ->
            ({ model | fishLocation = fishLocation }
                ! []
            )

        Save ->
            (add model ! [])

        Changevis ->
            (if model.isVisible == True then
                { model | isVisible = False }
                    ! []
             else
                { model | isVisible = True }
                    ! []
            )

        Mdl msg ->
            Material.update msg model



--_ ->
--  model ! []


add : Model -> Model
add model =
    let
        newfish =
            Fishes (List.length model.fishObject) model.caughtBy model.fishLocation model.fishBait

        newFishObject =
            newfish :: model.fishObject
    in
        { model | fishObject = newFishObject }


isVisible : Model -> String
isVisible model =
    if model.isVisible == True then
        "visible"
    else
        "hidden"



--View


type alias Mdl =
    Material.Model


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [] [ h1 [] [ text "Chris & James Most Excellent Fishing Adventure" ] ]
        , div []
            [ addFish model
            , addFishForm model
            , fishSection model
            ]
        ]


fishSection : Model -> Html Msg
fishSection model =
    div []
        [ fishSectionList model
          --  , fishTotal model
        ]


fishSectionHeader : Html Msg
fishSectionHeader =
    header []
        [ div [] [ text "Name" ]
        , div [] [ text "Bait" ]
        , div [] [ text "Location" ]
        ]


fishSectionList : Model -> Html Msg
fishSectionList model =
    model.fishObject
        |> List.map fishies
        |> Lists.ul []


fishies : Fishes -> Html Msg
fishies fishies =
    Lists.li []
        [ Lists.content [] [ text ((toString fishies.whoCaught) ++ " - " ++ (toString fishies.bait) ++ " - " ++ (toString fishies.location)) ]
        ]


addFish : Model -> Html Msg
addFish model =
    header []
        [ Button.render Mdl [ 0 ] model.mdl [ Button.raised, Button.colored, Button.onClick Changevis ] [ text "Caught a fish" ]
        ]
        |> Material.Scheme.top


addFishForm : Model -> Html Msg
addFishForm model =
    div [ class "sidenav", style [ ( "visibility", isVisible model ) ] ]
        [ Textfield.render Mdl
            [ 1 ]
            model.mdl
            [ Textfield.label "Who caught the Fish?", Textfield.floatingLabel, Textfield.onInput Caughtby ]
        , Textfield.render Mdl [ 2 ] model.mdl [ Textfield.label "What bait did you use?", Textfield.floatingLabel, Textfield.onInput Bait ]
        , Textfield.render Mdl [ 3 ] model.mdl [ Textfield.label "Where did you catch it?", Textfield.floatingLabel, Textfield.onInput Location ]
        , Button.render Mdl [ 4 ] model.mdl [ Button.raised, Button.colored, Button.onClick Save, Button.onClick Changevis ] [ text "Save" ]
        ]
