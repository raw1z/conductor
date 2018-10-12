module Route exposing (Route(..), fromUrl)

import Html exposing (Attribute)
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, int, map, oneOf, s, string)


type Route
    = Tasks


parser : Parser (Route -> a) a
parser =
    oneOf
        [ map Tasks (s "tasks")
        ]


fromUrl : Url -> Maybe Route
fromUrl url =
    Parser.parse parser url
