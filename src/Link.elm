module Link exposing (link)

import Html.Styled exposing (..)
import Html.Styled.Attributes
import Path
import Route exposing (Route(..))


link : Route -> List (Html.Styled.Attribute msg) -> Html.Styled.Html msg -> Html.Styled.Html msg
link route attrs label =
    Route.toLink
        (\routeAttrs ->
            Html.Styled.a
                (List.map Html.Styled.Attributes.fromUnstyled routeAttrs ++ attrs)
                [ label ]
        )
        route
