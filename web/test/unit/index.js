// Polyfill fn.bind() for PhantomJS
/* eslint-disable no-extend-native */
/* global beforeEach */

import Store from 'store'

Function.prototype.bind = require('function-bind')

beforeEach(function () {
  Store.currentUser.onNext(null)
})

// require all test files (files that ends with .spec.js)
var testsContext = require.context('.', true, /\.spec$/)
testsContext.keys().forEach(testsContext)
