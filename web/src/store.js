import _ from 'underscore'
import Rx from 'rx'

// One of two evils:
// store auth in cookie = CSRF
// store auth in localStorage = XSS
const localStorage = window.localStorage

var initialUser

try {
  initialUser = (
    localStorage &&
    localStorage.getItem('user') &&
    JSON.parse(localStorage.getItem('user'))) || null
} catch (_) {
  initialUser = null
}

const store = {
  isLoggedIn: false,
  currentUser: new Rx.BehaviorSubject(initialUser)
}

store.currentUser
  .subscribeOnNext(u => {
    store.isLoggedIn = !!u
    if (localStorage) {
      localStorage.setItem('user', JSON.stringify(u))
      _.isEmpty(u) && localStorage.removeItem('user')
    }
  })

export default store
