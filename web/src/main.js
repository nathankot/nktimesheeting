import _ from 'underscore'
import 'api'
import Store from 'store'
import Vue from 'vue'
import Router from 'vue-router'
import moment from 'moment'

Vue.use(Router)

const router = new Router()

import App from './components/app'
import Authenticate from './components/authenticate'
import Authenticated from './components/authenticated'
import Timesheet from './components/timesheet'
import TimesheetListView from './components/timesheetListView'
import Settings from './components/settings'
import Users from './components/users'

router.map({
  '/login': { component: Authenticate },
  '/': {
    component: Authenticated,
    subRoutes: {
      '/settings': { component: Settings },
      '/users': { component: Users },
      '/timesheet/:userId': {
        component: Timesheet,
        subRoutes: {
          '/list/:start/:end': {
            name: 'timesheet',
            component: TimesheetListView
          }
        }
      }
    }
  }
})

const defaultStart = moment().startOf('week').format('YYYY-MM-DD')
const defaultEnd = moment().endOf('week').format('YYYY-MM-DD')

router.alias({
})

router.redirect({
  '/timesheet': `/timesheet/0/list/${defaultStart}/${defaultEnd}`,
  '/': '/timesheet'
})

router.beforeEach(() => window.scrollTo(0, 0))

router.beforeEach((transition) => {
  if (transition.to.path !== '/login' && !Store.isLoggedIn) {
    return transition.redirect('/login')
  }
  transition.next()
})

Store.currentUser
  .scan((a, e) => [a[1], e], [null, null])
  .skip(1)
  // Listen to any changes to the current user
  .subscribeOnNext(([prev, next]) => {
    // Login
    if (_.isEmpty(prev) && !_.isEmpty(next)) {
      router.go('/')
    }

    // Logout
    if (!_.isEmpty(prev) && _.isEmpty(next)) {
      router.go('/login')
    }
  })

router.start(App, '#app')
