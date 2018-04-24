module Route exposing (Route(..), fromLocation, href, modifyUrl)

import Data.Article as Article
import Data.User as User exposing (Username)
import Html exposing (Attribute)
import Html.Attributes as Attr
import Navigation exposing (Location)
import UrlParser as Url exposing ((</>), Parser, oneOf, parseHash, s, string)


-- ROUTING --


type Route
    = Home
    | Root
    | Login
    | Test
    | Logout
    | Register
    | Settings
    | Article Article.Slug
    | Profile Username
    | NewArticle
    | EditArticle Article.Slug
    | Step1
    | Step2


route : Parser (Route -> a) a
route =
    oneOf
        [ Url.map Home (s "")
        , Url.map Login (s "login")
        , Url.map Test (s "test")
        , Url.map Logout (s "logout")
        , Url.map Settings (s "settings")
        , Url.map Profile (s "profile" </> User.usernameParser)
        , Url.map Register (s "register")
        , Url.map Article (s "article" </> Article.slugParser)
        , Url.map NewArticle (s "editor")
        , Url.map EditArticle (s "editor" </> Article.slugParser)
        , Url.map Step1 (s "step1")
        , Url.map Step2 (s "step2")
        ]



-- INTERNAL --


routeToString : Route -> String
routeToString page =
    let
        pieces =
            case page of
                Home ->
                    []

                Root ->
                    []

                Login ->
                    [ "login" ]

                Test ->
                    [ "test" ]

                Logout ->
                    [ "logout" ]

                Register ->
                    [ "register" ]

                Settings ->
                    [ "settings" ]

                Article slug ->
                    [ "article", Article.slugToString slug ]

                Profile username ->
                    [ "profile", User.usernameToString username ]

                NewArticle ->
                    [ "editor" ]

                EditArticle slug ->
                    [ "editor", Article.slugToString slug ]
                
                Step1 ->
                    [ "step1" ]
                
                Step2 ->
                    [ "step2" ]
    in
    "#/" ++ String.join "/" pieces



-- PUBLIC HELPERS --


href : Route -> Attribute msg
href route =
    Attr.href (routeToString route)


modifyUrl : Route -> Cmd msg
modifyUrl =
    routeToString >> Navigation.modifyUrl


fromLocation : Location -> Maybe Route
fromLocation location =
    if String.isEmpty location.hash then
        Just Root
    else
        parseHash route location
