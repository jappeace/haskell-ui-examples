{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module UI.Yesod (main) where

import Yesod
import Data.Text

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET
/about AboutR GET
/contact ContactR GET POST
|]

instance Yesod App
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

data ContactForm = ContactForm
    { name :: Text
    , message :: Textarea
    } deriving (Show)

contactForm :: Html -> MForm Handler (FormResult ContactForm, Widget)
contactForm = renderDivs $ ContactForm
    <$> areq textField "Name" Nothing
    <*> areq textareaField "Message" Nothing

getContactR :: Handler Html
getContactR = do
    (widget, enctype) <- generateFormPost contactForm
    defaultLayout $ do
        setTitle "Contact"
        [whamlet|
            <h1>Contact
            <p>Please fill out the form below.
            <form method=post enctype=#{enctype} action=@{ContactR}>
                ^{widget}
                <button>Submit
            <p>
              <a href=@{HomeR}>Home
              <br>
              <a href=@{AboutR}>About
        |]

postContactR :: Handler Html
postContactR = do
    ((result, widget), enctype) <- runFormPost contactForm
    case result of
        FormSuccess contact -> defaultLayout [whamlet|<h1>Thank you, #{name contact}!|]
        _ -> defaultLayout $ do
            setTitle "Contact - Error"
            [whamlet|
                <h1>Error
                <p>There was an error submitting the form. Please try again.
                <form method=post enctype=#{enctype} action=@{ContactR}>
                    ^{widget}
                    <button>Submit
                <p>
                  <a href=@{HomeR}>Home
                  <br>
                  <a href=@{AboutR}>About
            |]

getHomeR :: Handler Html
getHomeR = defaultLayout
  [whamlet|
    <h1>Welcome to the Home page
    <p>
      <a href=@{AboutR}>About
      <br>
      <a href=@{ContactR}>Contact
  |]

getAboutR :: Handler Html
getAboutR = defaultLayout
  [whamlet|
    <h1>About
    <p>This is the About page.
    <p>
      <a href=@{HomeR}>Home
      <br>
      <a href=@{ContactR}>Contact
  |]

main :: IO ()
main = warp 3000 App
