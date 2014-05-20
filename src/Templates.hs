{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Templates
  ( indexTpl
  , showTpl
  )
where

import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Hamlet
import Data.Text.Lazy
import Text.Blaze.Internal (Markup)

base :: Markup -> Markup
base body = [shamlet|
<html>
  <head>
    <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <link rel="stylesheet" type="text/css" href="//netdna.bootstrapcdn.com/bootstrap/3.1.1/css/bootstrap.min.css">
    <title>
      9m – URL Shortener
  <body>
    <div class="container">
      #{header}
      #{body}
      <script src="//ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min.js">
      <script src="//netdna.bootstrapcdn.com/bootstrap/3.1.1/js/bootstrap.min.js">
|]

header :: Markup
header = [shamlet|
<div class="col-md-offset-3 col-md-6 col-xs-12">
  <h1 class="text-center">9m – A URL Shortener for the Unicode Age 💻
|]

indexTpl :: Text
indexTpl = renderHtml $ base body
  where body = [shamlet|
<div class="col-md-offset-3 col-md-6 col-xs-12">
  <form role="form" method="post" action="/create">
    <div class="form-group">
      <div class="row">
        <input type="url" class="form-control" name="url" id="inputUrl" placeholder="URL to shorten">
      <div class="row">
        <p class="text-center">
          <button type="submit" class="btn btn-primary btn-lg btn-block">Create short link</button>
|]


showTpl :: Text -> Text -> Text
showTpl key value = renderHtml $ base body
  where body = [shamlet|
<div class="col-md-offset-3 col-md-6 col-xs-12">
  <div class="row">
    <a href="http://9m.no/#{key}">
      http://9m.no/#{key}
    →
    <a href="#{value}">
      #{value}
    <button type="button" class="btn btn-success">📋 Copy</button>
|]
