<template>
  <div class="header">
    <div class="current-user">
      Logged in as {{ user.email }}
    </div>

    <div class="menu-and-title">
      <nav class="account-menu">
        <ul>
          <li><a v-link="{ path: '/timesheet' }">timesheet</a></li>
          <li><a v-link="{ path: '/settings' }">settings</a></li>
          <li v-if="usersEditable"><a v-link="{ path: '/users' }">users</a></li>
          <li><a @click="logout">logout</a></li>
        </ul>
      </nav>

      <h1 class="title">nktimesheeting</h1>
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

   computed: {
     usersEditable () {
       return _.isArray(this.user.roles) &&
              _.contains(this.user.roles, 'Admin')
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

<style lang="sass">
 @import 'src/settings';
 @import 'src/mixins';

 .header {
   @include outer-container;
   border-bottom: 1px solid #E8E8E8;

   .current-user {
     @include span-columns(12);
     color: #CDCDCD;
     @include media($mobile) {
       text-align: center;
     }
   }

   .account-menu {
     @include span-columns(8);

     ul {
       padding: 0;
       margin: 0;
       list-style: none;
     }

     li {
       display: inline-block;
       margin-right: em(12);
     }

     .v-link-active {
       text-decoration: none;
     }

     @include media($mobile) {
       text-align: center;
       li {
         margin: 0 em(6);
       }
     }
   }

   .title {
     @include span-columns(4);
     margin: 0;
     text-align: right;
     font-size: em(24);
   }

   .menu-and-title {
     @include row(block, RTL);
   }

   .account-menu, .title {
     line-height: 56px;
     @include media($mobile) {
       @include span-columns(6);
       text-align: center;
     }
   }
 }
</style>
