import _ from 'underscore'
import 'api'
import Store from 'store'
import Vue from 'vue'
import Router from 'vue-router'

Vue.use(Router)

const router = new Router()

import App from './components/app'
import Authenticate from './components/authenticate'
import Authenticated from './components/authenticated'
import Timesheet from './components/timesheet'
import Settings from './components/settings'

router.map({
  '/login': { component: Authenticate },
  '/': {
    component: Authenticated,
    subRoutes: {
      '/timesheet': { component: Timesheet },
      '/settings': { component: Settings }
    }
  }
})

router.beforeEach(() => window.scrollTo(0, 0))

router.beforeEach((transition) => {
  if (transition.to.path !== '/login' && !Store.isLoggedIn) {
    return transition.redirect('/login')
  }
  transition.next()
})

Store.currentUser
  .subscribeOnNext((u) => {
    if (!_.isEmpty(u)) {
      router.go('/')
    } else {
      router.go('/login')
    }
  })

router.redirect({
  '*': '/login',
  '/': '/timesheet'
})

router.start(App, '#app')
