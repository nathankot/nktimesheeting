import _ from 'underscore'
import Rx from 'rx'
import moment from 'moment-timezone'

// One of two evils:
// store auth in cookie = CSRF
// store auth in localStorage = XSS
const localStorage = window.localStorage

var initialUser =
    (localStorage && localStorage.getItem('user')) ||
    null

var initialTimezone =
    (localStorage && localStorage.getItem('timezone')) ||
    moment.tz.guess()

const store = {
  isLoggedIn: false,
  currentUser: new Rx.BehaviorSubject(initialUser),
  timezone: new Rx.BehaviorSubject(initialTimezone)
}

store.currentUser
  .subscribeOnNext(u => {
    store.isLoggedIn = !!u
    if (localStorage) {
      localStorage.setItem('user', u)
      _.isEmpty(u) && localStorage.removeItem('user')
    }
  })

store.timezone
  .subscribeOnNext(z => {
    if (localStorage) {
      localStorage.setItem('timezone', z)
    }
  })

export default store
