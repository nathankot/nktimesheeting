<template>
  <div class="header">
    <div class="account-details">
      <div class="current-user">
        Logged in as {{ user.email }}
      </div>
      
      <div class="account-menu">
        <ul>
          <li><a v-link="{ path: '/settings' }">settings</a></li>
          <li><a @click="logout">logout</a></li>
        </ul>
      </div>

      <h1>nktimesheeting</h1>
    </div>
  </div>
</template>

<script>
 import _ from 'underscore'
 import Rx from 'rx'
 import Store from 'store'
 import Api from 'api'

 export default {
   data () {
     return {
       disposable: new Rx.CompositeDisposable(),
       user: { email: '' }
     }
   },

   attached () {
     this.disposable.add(
       Store.currentUser
            .filter((u) => _.isObject(u))
            .subscribeOnNext((u) => this.user = u))
   },

   methods: {
     logout () {
       Store.currentUser.onNext(null)
       Api.sessions.delete()
     }
   },

   beforeDestroy () {
     this.disposable.dispose()
   }
 }
</script>