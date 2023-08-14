{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Templates
  ( indexTpl,
    showTpl,
    selfTpl,
    aboutTpl,
  )
where

import Data.Maybe (fromJust, isJust)
import Data.Text.Lazy (Text)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Internal (Markup)
import Text.Hamlet

base :: Maybe Text -> Markup -> Maybe Text -> Markup
base subTitle body onloadAction =
  [shamlet|
$doctype 5
<html>
  <head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <link rel="stylesheet" type="text/css" href="//netdna.bootstrapcdn.com/bootstrap/3.1.1/css/bootstrap.min.css">
    <script type="text/javascript">
      // Script snippet courtesy of user 'Igal' who posted this on StackOverflow at
      // http://stackoverflow.com/questions/1173194/select-all-div-text-with-single-mouse-click
      function selectText( containerid ) {
          var node = document.getElementById( containerid );
          if ( document.selection ) {
              var range = document.body.createTextRange();
              range.moveToElementText( node  );
              range.select();
          } else if ( window.getSelection ) {
              var range = document.createRange();
              range.selectNodeContents( node );
              window.getSelection().removeAllRanges();
              window.getSelection().addRange( range );
          }
      }
      function copyShortUrlToClipboardAndNotifyUser() {
          selectText( 'shortenedUrl' );
          document.execCommand( 'copy' );
          document.getElementById( 'message' ).innerText = "Copied to clipboard!";
      }
    <style>
      h1 {
        font-weight: bolder;
        font-size: 5em;
      }
      div.header {
        margin-bottom: 50px;
      }
      div.result {
        padding: 30px;
        border-radius: 10px;
        background-color: #ECECEC;
      }
      .large {
        font-size: 2em;
      }
      .truncate {
        width: 100%;
        white-space: nowrap;
        overflow: hidden;
        text-overflow: ellipsis;
      }
      hr {
        margin-bottom: 3px;
      }
      a.clipboard:link, a.clipboard:hover {
        text-decoration: none;
      }
    <title>
      $if isJust subTitle
        9m URL shortener – #{(fromJust subTitle)}
      $else
      9m URL shortener
  <body :isJust onloadAction:onload="#{fromJust onloadAction}">
    <div class="container">
      #{header}
      #{body}
      #{footer}
      <script src="//ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min.js">
      <script src="//netdna.bootstrapcdn.com/bootstrap/3.1.1/js/bootstrap.min.js">
      <script data-goatcounter="https://ninem.goatcounter.com/count" async src="//gc.zgo.at/count.js"></script>
|]

header :: Markup
header =
  [shamlet|
<div class="col-md-offset-3 col-md-6 col-xs-12 header">
  <h1 class="text-center">
    <a href="/">
      #{logo}
|]

footer :: Markup
footer =
  [shamlet|
<div class="row">
  <div class="col-md-offset-3 col-md-6 col-xs-12">
    <hr>
<div class="row">
  <div class="col-md-offset-3 col-md-6 col-xs-12 text-center">
    <a href="https://www.github.com/ehamberg/9m">
      Code
    ◇
    <a href="/about">
      About
|]

indexTpl :: Text
indexTpl = renderHtml $ base Nothing body (Just "document.forms[0].url.focus();")
  where
    body =
      [shamlet|
<div class="col-md-offset-3 col-md-6 col-xs-12">
  <div class="row">
    <div class="col-md-12">
      <form role="form" method="post" action="/create">
        <div class="input-group input-group-lg">
          <input type="text" class="form-control" name="url" placeholder="URL to shorten">
          <span class="input-group-btn">
            <button class="btn btn-primary" type="submit">Shorten</button>
|]

selfTpl :: Text
selfTpl = renderHtml $ base Nothing body Nothing
  where
    body =
      [shamlet|
<div class="col-md-offset-2 col-md-8 col-xs-12 result">
  <div class="row text-center large">
    https://9m.no/
  <div class="row text-center large">
    ↻
|]

showTpl :: Text -> Text -> Text
showTpl key url = renderHtml $ base Nothing body (Just "selectText('shortenedUrl');")
  where
    body =
      [shamlet|
<div class="col-md-offset-2 col-md-8 col-xs-12 result">
  <div class="row text-center large">
    <a href="https://9m.no/#{key}" id="shortenedUrl">https://9m.no/#{key}</a>
    <a href="javascript:void(0);" class="clipboard" onclick="copyShortUrlToClipboardAndNotifyUser();">📋</a>
  <div class="row text-center large" id="message">
  <div class="row text-center large">
    ⇩
  <div class="row text-center large truncate">
    #{url}
|]

aboutTpl :: Text
aboutTpl = renderHtml $ base (Just "About") body Nothing
  where
    body =
      [shamlet|
<div class="row">
  <div class="col-md-offset-3 col-md-6 col-xs-12">
    <h2 class="text-center">
      About 9m
<div class="row">
  <div class="col-md-offset-3 col-md-6 col-xs-12">
    <ul>
      <li>
        Made slightly – but not completely – tongue-in-cheek by
        <a href="https://twitter.com/ehamberg">@ehamberg</a>.
      <li>
        Picks two random unicode code points between ‘A’ (<code>U+0041
        LATIN CAPITAL LETTER A</code>) and ‘🛅’ (<code>U+1F6C5 LEFT
        LUGGAGE</code>) for the short URL, which seems to be an okayish
        compromise between having many characters and having some hope of
        being supported by a modern font.
      <li>
        The number of printable characters between <code>U+0041</code> and
        <code>U+1F6C5</code> is 61229, according to GHC 7.8.2:
        <pre>
          λ> (length . filter isPrint) [chr 0x41 .. chr 0x1F6C5]
          61229
      <li>
        The
        <a href="https://github.com/ehamberg/9m">
          source code
        is on github.
|]

logo :: Markup
logo =
  [shamlet|
<svg width="80%" viewBox="0 0 187.93623 48.856403" xmlns="http://www.w3.org/2000/svg" xmlns:svg="http://www.w3.org/2000/svg">
  <g stroke="none" stroke-width="1" fill="none" fill-rule="evenodd" transform="translate(-0.91308594,-0.72799999)">
    <path d="m 69.976,26.36 h -6.48 c 0.192001,1.472007 0.591997,2.783994 1.2,3.936 0.608003,1.152006 1.391995,2.127996 2.352,2.928 0.960005,0.800004 2.063994,1.399998 3.312,1.8 1.248006,0.400002 2.591993,0.6 4.032,0.6 2.400012,0 4.431992,-0.519995 6.096,-1.56 1.664008,-1.040005 3.023995,-2.399992 4.08,-4.08 1.056005,-1.680008 1.823998,-3.55999 2.304,-5.64 0.480002,-2.08001 0.72,-4.15999 0.72,-6.24 0,-2.112011 -0.191998,-4.20799 -0.576,-6.288 C 86.631998,9.7359896 85.944005,7.8720082 84.952,6.224 83.959995,4.5759918 82.640008,3.248005 80.992,2.24 79.343992,1.231995 77.240013,0.728 74.68,0.728 c -1.760009,0 -3.359993,0.3199968 -4.8,0.96 -1.440007,0.6400032 -2.679995,1.5119945 -3.72,2.616 -1.040005,1.1040055 -1.839997,2.3999926 -2.4,3.888 -0.560003,1.4880074 -0.84,3.095991 -0.84,4.824 0,1.344007 0.207998,2.671993 0.624,3.984 0.416002,1.312007 1.071995,2.479995 1.968,3.504 0.960005,1.056005 2.127993,1.895997 3.504,2.52 1.376007,0.624003 2.783993,0.936 4.224,0.936 1.632008,0 3.071994,-0.287997 4.32,-0.864 1.248006,-0.576003 2.335995,-1.535993 3.264,-2.88 l 0.096,0.096 c -0.064,0.928005 -0.199999,1.983994 -0.408,3.168 -0.208001,1.184006 -0.551998,2.295995 -1.032,3.336 -0.480002,1.040005 -1.119996,1.919996 -1.92,2.64 -0.800004,0.720004 -1.823994,1.08 -3.072,1.08 -1.184006,0 -2.175996,-0.415996 -2.976,-1.248 -0.800004,-0.832004 -1.311999,-1.807994 -1.536,-2.928 z m 4.896,-7.536 c -0.864004,0 -1.623997,-0.183998 -2.28,-0.552 -0.656003,-0.368002 -1.191998,-0.847997 -1.608,-1.44 -0.416002,-0.592003 -0.727999,-1.263996 -0.936,-2.016 -0.208001,-0.752004 -0.312,-1.527996 -0.312,-2.328 0,-0.768004 0.111999,-1.519996 0.336,-2.256 0.224001,-0.7360037 0.559998,-1.3919971 1.008,-1.968 0.448002,-0.5760029 0.991997,-1.0399982 1.632,-1.392 0.640003,-0.3520018 1.359996,-0.528 2.16,-0.528 0.864004,0 1.631997,0.1759982 2.304,0.528 0.672003,0.3520018 1.239998,0.823997 1.704,1.416 0.464002,0.592003 0.815999,1.2639962 1.056,2.016 0.240001,0.752004 0.36,1.511996 0.36,2.28 0,0.832004 -0.111999,1.623996 -0.336,2.376 -0.224001,0.752004 -0.559998,1.415997 -1.008,1.992 -0.448002,0.576003 -1.007997,1.031998 -1.68,1.368 -0.672003,0.336002 -1.471995,0.504 -2.4,0.504 z m 13.512,-8.64 V 35 H 95.2 V 20.6 c 0,-1.216006 0.175998,-2.199996 0.528,-2.952 0.352002,-0.752004 0.775997,-1.327998 1.272,-1.728 0.496003,-0.400002 1.007997,-0.671999 1.536,-0.816 0.528003,-0.144001 0.951998,-0.216 1.272,-0.216 1.08801,0 1.912,0.183998 2.472,0.552 0.56,0.368002 0.96,0.855997 1.2,1.464 0.24,0.608003 0.376,1.271996 0.408,1.992 0.032,0.720004 0.048,1.447996 0.048,2.184 V 35 h 6.816 V 21.176 c 0,-0.768004 0.056,-1.527996 0.168,-2.28 0.112,-0.752004 0.336,-1.423997 0.672,-2.016 0.336,-0.592003 0.8,-1.071998 1.392,-1.44 0.592,-0.368002 1.368,-0.552 2.328,-0.552 0.96,0 1.72,0.159998 2.28,0.48 0.56,0.320002 0.984,0.751997 1.272,1.296 0.288,0.544003 0.464,1.183996 0.528,1.92 0.064,0.736004 0.096,1.519996 0.096,2.352 V 35 h 6.816 V 18.392 c 0,-1.600008 -0.224,-2.967994 -0.672,-4.104 -0.448,-1.136006 -1.072,-2.055997 -1.872,-2.76 -0.8,-0.704003 -1.75999,-1.215998 -2.88,-1.536 -1.12001,-0.3200016 -2.33599,-0.48 -3.648,-0.48 -1.72801,0 -3.22399,0.4159958 -4.488,1.248 -1.26401,0.832004 -2.264,1.791995 -3,2.88 -0.672,-1.536008 -1.65599,-2.607997 -2.952,-3.216 -1.29601,-0.608003 -2.72799,-0.912 -4.296,-0.912 -1.63201,0 -3.079994,0.3519965 -4.344,1.056 -1.264006,0.704004 -2.343996,1.695994 -3.24,2.976 h -0.096 v -3.36 z" fill="#468cc8" />
    <path d="m 129.91309,35.737305 0.0342,-32.0605472 h 5.91308 l -0.0342,32.0605472 z M 140.06445,19.707031 156.09473,3.6767578 V 16.733398 h 32.06054 l -0.0342,5.913086 h -32.02636 l -0.0342,13.090821 z" fill="#d4d4d4" />
    <path d="M 59.121094,3.6767578 59.086914,35.737305 H 53.173828 L 53.208008,3.6767578 Z M 48.969727,19.707031 32.939453,35.737305 V 22.646484 H 0.91308594 V 16.733398 H 32.973633 V 3.6767578 Z" fill="#d4d4d4" />
  <text xml:space="preserve" font-family="Helvetica, sans-serif" text-anchor="middle" style="font-size:8.51907px;fill:#000000;stroke-width:0.709923" x="93.968115" y="44.096848">
    Ridiculously Short URLs for the Unicode Age
|]
