{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Templates
  ( indexTpl
  , showTpl
  , selfTpl
  , aboutTpl
  )
where

import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Hamlet
import Data.Text.Lazy
import Text.Blaze.Internal (Markup)
import Data.Maybe (fromJust, isJust)

base :: Markup -> Maybe Text -> Markup
base body onloadAction = [shamlet|
$doctype 5
<html>
  <head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <link rel="stylesheet" type="text/css" href="//netdna.bootstrapcdn.com/bootstrap/3.1.1/css/bootstrap.min.css">
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
    <title>
      9m URL Shortener
  <body :isJust onloadAction:onload="#{fromJust onloadAction}">
    <div class="container">
      #{header}
      #{body}
      #{footer}
      <script src="//ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min.js">
      <script src="//netdna.bootstrapcdn.com/bootstrap/3.1.1/js/bootstrap.min.js">
      <script>
        (function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
        (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
        m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
        })(window,document,'script','//www.google-analytics.com/analytics.js','ga');

        ga('create', 'UA-304186-3', '9m.no');
        ga('send', 'pageview');
|]

header :: Markup
header = [shamlet|
<div class="col-md-offset-3 col-md-6 col-xs-12 header">
  <h1 class="text-center">
    <a href="/">
      <img alt="9m" width="80%" src="/static/svg/logo.svg">
|]

footer :: Markup
footer = [shamlet|
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
indexTpl = renderHtml $ base body Nothing
  where body = [shamlet|
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
selfTpl = renderHtml $ base body Nothing
  where body = [shamlet|
<div class="col-md-offset-2 col-md-8 col-xs-12 result">
  <div class="row text-center large">
    http://9m.no/
  <div class="row text-center large">
    ↻
|]

showTpl :: Text -> Text -> Text
showTpl key url = renderHtml $ base body Nothing
  where body = [shamlet|
<div class="col-md-offset-2 col-md-8 col-xs-12 result">
  <div class="row text-center large">
    http://9m.no/#{key}
  <div class="row text-center large">
    ⇩
  <div class="row text-center large truncate">
    #{url}
|]

aboutTpl :: Text
aboutTpl = renderHtml $ base body Nothing
  where body = [shamlet|
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
