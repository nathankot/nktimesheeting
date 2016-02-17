import Rx from 'rx'

const store = {
  isLoggedIn: false,
  currentUser: new Rx.BehaviorSubject(null)
}

store.currentUser
  .subscribeOnNext(u => {
    store.isLoggedIn = !!u
  })

export default store
