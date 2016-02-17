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

      <div class="current-timezone">
        <select
            v-model="timezone"
            @change="updateTimezone(timezone)">
          <option
              v-for="zone in availableTimezones"
              :value="zone">
            {{ zone }}
          </option>
        </select>
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
 import moment from 'moment-timezone'

 export default {
   data () {
     return {
       disposable: new Rx.CompositeDisposable(),
       user: { email: '' },
       timezone: 'UTC',
       availableTimezones: moment.tz.names()
     }
   },

   attached () {
     this.disposable.add(
       Store.currentUser
            .filter((u) => _.isObject(u))
            .subscribeOnNext((u) => this.user = u))

     this.disposable.add(
       Store.timezone.take(1)
         .subscribeOnNext((z) => this.timezone = z))
   },

   methods: {
     updateTimezone (z) {
       Store.timezone.onNext(z)
     },
     logout () {
       this.disposable.add(
         Api.sessions.delete().rx()
            .subscribeOnNext(() => Store.currentUser.onNext(null)))
     }
   },

   beforeDestroy () {
     this.disposable.dispose()
   }
 }
</script>
