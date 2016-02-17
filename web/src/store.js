import Rx from 'rx'
// import _ from 'underscore'

const store = {
  currentUser: new Rx.Subject(null)
}

store.currentUser
  .subscribeOnNext(u => {
    // Reset user-specific data on logout
  })

export default store
