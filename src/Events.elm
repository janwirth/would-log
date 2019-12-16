module Events exposing (..)

{-| Additional Events for elm-ui
-}

import Element
import Html.Events
import Json.Decode


{-| Replicates the onClick function but prevents bubbling
-}
onEnterKeyDown : msg -> Element.Attribute msg
onEnterKeyDown message =
    Json.Decode.field "key" Json.Decode.string
        |> Json.Decode.andThen (\keyName -> if keyName == "Enter" then Json.Decode.succeed { stopPropagation = True, preventDefault = True, message = message } else Json.Decode.fail "Enter not pressed")
        |> Html.Events.custom "keydown"
        |> Element.htmlAttribute

