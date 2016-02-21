<template>

  <div class="users-list">
    <table>
      <thead>
        <tr>
          <th>Email</th>
          <th>Manager</th>
          <th>Admin</th>
          <th></th>
        </tr>
      </thead>
      <tbody>
        <tr v-for="user in sortedUsers">
          <td>{{ user.email }}</td>
          <td>
            <input
                type="checkbox"
                value="Manager"
                v-model="user.roles"
                @change="updateRolesFor(user)" />
          </td>
          <td>
            <input
                type="checkbox"
                value="Admin"
                v-model="user.roles"
                @change="updateRolesFor(user)" />
          </td>
          <td>
            <a @click="deleteUser(user)">Delete</a>
          </td>
        </tr>
      </tbody>
    </table>
  </div>

</template>

<script>
 import _ from 'underscore'
 import Api from 'api'
 import Store from 'store'
 import Rx from 'rx'

 export default {
   data () {
     return {
       _: _,
       refreshSignal: new Rx.Subject(),
       disposable: new Rx.CompositeDisposable(),
       users: []
     }
   },

   computed: {
     sortedUsers () {
       return _.sortBy(this.users, 'id')
     }
   },

   methods: {
     updateRolesFor (user) {
       this.disposable.add(
         Api.users.update({ id: user.id }, user)
            .rx()
            // Reload the users if things went wrong
            .subscribeOnError(() => this.refreshSignal.onNext()))
     },

     deleteUser (user) {
       this.disposable.add(
         Api.users.delete({ id: user.id })
            .rx()
           .doOnNext(() => this.users = _.filter(this.users, (u) => u.id !== user.id))
           .subscribeOnError(() => this.refreshSignal.onNext()))
     }
   },

   ready () {
     this.disposable.add(
       Rx.Observable.combineLatest(
         Store.currentUser,
         this.refreshSignal
             .startWith(true)
             .map(() => Api.users.get().rx())
             .switchLatest())
         .subscribeOnNext(({ 0: currentUser, 1: res }) => {
           this.users = _.filter(res.data.users, (u) => {
             return u.id !== currentUser.id
           })
         }))
   }
 }
</script>