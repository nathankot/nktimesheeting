var express = require('express')
var webpack = require('webpack')
var config = require('./webpack.dev.conf')

var app = express()
var compiler = webpack(config)
var request = require('request');

// proxy api requests to the server running on 3000
app.use(function (req, res, next) {
  if (req.url.match(/^\/api\//) !== null ) {
    req.pipe(request("http://localhost:3000" + req.url)).pipe(res)
  } else {
    next()
  }
})

// handle fallback for HTML5 history API
app.use(require('connect-history-api-fallback')())

// serve webpack bundle output
app.use(require('webpack-dev-middleware')(compiler, {
  publicPath: config.output.publicPath,
  stats: {
    colors: true,
    chunks: false
  }
}))

// enable hot-reload and state-preserving
// compilation error display
app.use(require('webpack-hot-middleware')(compiler))

app.listen(8080, 'localhost', function (err) {
  if (err) {
    console.log(err)
    return
  }
  console.log('Listening at http://localhost:8080')
})
