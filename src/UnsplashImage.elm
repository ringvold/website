module UnsplashImage exposing (UnsplashImage, decoder, default, fromId, image, imagePath, rawUrl)

import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attr exposing (css)
import OptimizedDecoder as Decode exposing (Decoder)
import Pages.Url
import Url.Builder exposing (string)


type UnsplashImage
    = UnsplashImage String


default : UnsplashImage
default =
    fromId "1587382668076-5101b7cd8eae"


image :
    List (Html.Attribute msg)
    -> UnsplashImage
    -> Html msg
image attrs (UnsplashImage url_) =
    Html.img
        ([ Attr.src url_ ] ++ attrs)
        []


imagePath : UnsplashImage -> Pages.Url.Url
imagePath (UnsplashImage url_) =
    url_
        |> Pages.Url.external


rawUrl : UnsplashImage -> String
rawUrl (UnsplashImage url_) =
    url_


fromId : String -> UnsplashImage
fromId id =
    UnsplashImage (url id)


decoder : Decoder UnsplashImage
decoder =
    Decode.string
        |> Decode.map (url >> UnsplashImage)


url : String -> String
url photoId =
    Url.Builder.crossOrigin
        "https://images.unsplash.com"
        [ "photo-" ++ photoId ]
        [ string "auto" "format"
        , string "fit" "crop"
        , Url.Builder.int "w" 720
        , Url.Builder.int "h" 400
        , Url.Builder.int "q" 80
        ]
