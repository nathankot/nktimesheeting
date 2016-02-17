import Vue from 'vue'
import Router from 'vue-router'

Vue.use(Router)

const router = new Router()

import App from './components/app'
import Authenticate from './components/authenticate'

router.map({
  '/login': { component: Authenticate }
})

router.beforeEach(() => window.scrollTo(0, 0))
router.redirect({ '*': '/login' })

router.start(App, '#app')
