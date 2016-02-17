import Rx from 'rx'
// import _ from 'underscore'

const store = {
  isLoggedIn: false,
  currentUser: new Rx.Subject(null)
}

store.currentUser
  .subscribeOnNext(u => {
    store.isLoggedIn = !!u
  })

export default store
