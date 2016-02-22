<template>

  <form @submit.prevent="submit" class="form">
    <error-message v-show="error">{{ error }}</error-message>

    <div class="field-group of-two">
      <div class="field">
        <div class="label">Email</div>
        <input v-model="email" type="email" placeholder="your@email.com" />
      </div>

      <div class="field">
        <div class="label">Password</div>
        <input v-model="password" type="password" placeholder="password" />
      </div>
    </div>

    <input class="submit" type="submit" value="Login" />
  </form>

</template>

<script>
 import Rx from 'rx'
 import Store from 'store'
 import Api, { getError } from 'api'

 import ErrorMessage from './errorMessage'

 export default {
   data () {
     return {
       disposable: new Rx.CompositeDisposable(),
       email: '',
       password: '',
       error: null
     }
   },
   methods: {
     submit () {
       this.error = null
       this.disposable.add(
        Api.sessions
          .save({ email: this.email, password: this.password })
          .rx()
          .doOnNext(r => Store.currentUser.onNext(r.data.user))
          .subscribeOnError(r => { this.error = getError(r) }))
     }
   },
   components: { ErrorMessage },
   beforeDestroy () {
     this.disposable.dispose()
   }
 }
</script>
