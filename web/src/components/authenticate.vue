<template>

  <div class="login">
    <h2>Login</h2>
    <login-form></login-form>
  </div>

  <div class="register">
    <h2>Register</h2>
    <user-form></user-form>
  </div>

</template>

<script>
 import LoginForm from './loginForm'
 import UserForm from './userForm'

 export default {
   components: { LoginForm, UserForm }
 }
</script>

<style lang="sass">
 @import 'src/settings';
 @import 'src/mixins';

 .login, .register {
   @include outer-container();
   margin-bottom: em(72);
 }
</style>
