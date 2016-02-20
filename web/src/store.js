import Rx from 'rx'

// One of two evils:
// store auth in cookie = CSRF
// store auth in localStorage = XSS
const localStorage = window.localStorage

const store = {
  isLoggedIn: false,
  currentUser: new Rx.BehaviorSubject(fromLocalStorage('user')),
  preferredWorkingHoursPerDay: new Rx.BehaviorSubject(fromLocalStorage('preferredWorkingHours', 8))
}

store.currentUser
  .subscribeOnNext(u => {
    store.isLoggedIn = !!u
    toLocalStorage('user', u)
  })

store.preferredWorkingHoursPerDay
  .subscribeOnNext(h => toLocalStorage('preferredWorkingHours', h))

function toLocalStorage (key, value) {
  if (localStorage && JSON && JSON.stringify) {
    localStorage.setItem(key, JSON.stringify(value))
  }
}

function fromLocalStorage (key, def = null) {
  try {
    return (localStorage &&
            localStorage.getItem(key) &&
            JSON && JSON.parse &&
            JSON.parse(localStorage.getItem(key))) || def
  } catch (_) {
    return null
  }
}

export default store

