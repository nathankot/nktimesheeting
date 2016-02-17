<template>
  <error-message v-show="showError">{{ error }}</error-message>
  <input v-model="user.email" type="email" placeholder="email@address.com"/>
  <input v-model="user.password" type="password" placeholder="choose a password" />
  <button @click="submit" class="submit">{{ submitButtonLabel }}</button>
</template>

<script>
 import _ from 'underscore'
 import Api, { getError } from 'api'
 import Store from 'store'

 import ErrorMessage from './errorMessage'

 export default {
   props: {
     isRegister: {
       type: Boolean,
       default: true
     },
     user: {
       type: Object,
       default () {
         return {
           email: '',
           password: ''
         }
       }
     }
   },
   data () {
     return {
       error: ''
     }
   },
   computed: {
     showError () { return !_.isEmpty(this.error) },
     submitButtonLabel () { return this.isRegister ? 'Register' : 'Update' }
   },
   methods: {
     submit () {
       this.error = null
       Api.users[this.isRegister ? 'save' : 'update'](this.user).rx()
          .doOnNext((res) => {
            if (this.isRegister) {
              Store.currentUser.onNext(res.data.user)
            }
          })
         .subscribeOnError((res) => this.error = getError(res))
     }
   },
   components: { ErrorMessage }
 }
</script>
