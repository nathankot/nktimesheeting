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

 import ErrorMessage from './errorMessage'

 function defaultUser () {
   return {
     id: null,
     email: '',
     password: ''
   }
 }

 export default {
   props: {
     onCreate: { type: Function, default: _.noop },
     createLabel: { default: null },
     user: {
       type: Object,
       default () { return defaultUser() }
     }
   },
   data () {
     return {
       error: '',
       disposable: new Rx.CompositeDisposable()
     }
   },
   computed: {
     isCreate () { return _.isEmpty(this.user.id) },
     showError () { return !_.isEmpty(this.error) },
     submitButtonLabel () {
       if (_.isString(this.createLabel)) { return this.createLabel }
       return this.isCreate ? 'Register' : 'Update'
     }
   },
   methods: {
     submit () {
       this.error = null
       this.disposable.add(
         Api.users[this.isCreate ? 'save' : 'update'](this.user).rx()
            .doOnNext((res) => {
              if (this.isCreate) {
                this.onCreate(res.data.user)
                this.user = defaultUser()
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
