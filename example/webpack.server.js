#!/usr/bin/env node

const serve = require('webpack-serve')
const compress = require('compression')
const convert = require('koa-connect')

const config = require('./webpack.development')

serve({
  config,
  logLevel: 'info',
  port: 6767,
  add: (app, middleware) => {
    middleware.webpack()
    middleware.content()

    app.use(convert(compress()))
  },
})
