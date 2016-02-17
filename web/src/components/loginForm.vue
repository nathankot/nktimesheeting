<template>
  <error-message v-show="showError">{{ error }}</error-message>
  <input v-model="email" type="email" placeholder="your@email.com" />
  <input v-model="password" type="password" placeholder="password" />
  <button @click="submit" class="submit">Login</button>
</template>

<script>
 import Store from 'store'
 import Api, { getError } from 'api'
 import _ from 'underscore'

 import ErrorMessage from './errorMessage'

 export default {
   data () {
     return {
       email: '',
       password: '',
       error: null
     }
   },
   computed: {
     showError () { return !_.isEmpty(this.error) }
   },
   methods: {
     submit () {
       this.error = null
       Api.sessions
          .save({ email: this.email, password: this.password })
          .rx()
          .doOnNext(r => Store.currentUser.onNext(r.data.user))
          .subscribeOnError(r => { this.error = getError(r) })
     }
   },
   components: { ErrorMessage }
 }
</script>
