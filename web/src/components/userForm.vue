<template>

  <form @submit.prevent="submit" class="form">
    <error-message v-show="showError">{{ error }}</error-message>

    <div class="field-group of-two">
      <div class="field">
        <div class="label">Email</div>
        <input v-model="user.email" type="email" placeholder="email@address.com"/>
      </div>

      <div class="field">
        <div class="label">Password</div>
        <input v-model="user.password" type="password" placeholder="choose a password" />
      </div>
    </div>

    <input class="submit" type="submit" :value="submitButtonLabel" />
  </form>

</template>

<script>
 import Rx from 'rx'
 import _ from 'underscore'
 import Api, { getError } from 'api'
 import Store from 'store'

 import ErrorMessage from './errorMessage'

 export default {
   props: {
     user: {
       type: Object,
       default () {
         return {
           id: null,
           email: '',
           password: ''
         }
       }
     }
   },
   data () {
     return {
       error: '',
       disposable: new Rx.CompositeDisposable()
     }
   },
   computed: {
     isRegister () { return _.isEmpty(this.user.id) },
     showError () { return !_.isEmpty(this.error) },
     submitButtonLabel () { return this.isRegister ? 'Register' : 'Update' }
   },
   methods: {
     submit () {
       this.error = null
       this.disposable.add(
         Api.users[this.isRegister ? 'save' : 'update'](this.user).rx()
            .doOnNext((res) => {
              if (this.isRegister) {
                Store.currentUser.onNext(res.data.user)
              }
            })
            .subscribeOnError((res) => this.error = getError(res)))
     }
   },
   components: { ErrorMessage },
   beforeDestroy () {
     this.disposable.dispose()
   }
 }
</script>
