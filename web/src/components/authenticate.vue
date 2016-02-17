<template>

  <h2>Login</h2>
  <login-form></login-form>

  <h2>Register</h2>
  <user-form></user-form>

</template>

<script>
 import LoginForm from './loginForm'
 import UserForm from './userForm'

 export default {
   components: { LoginForm, UserForm }
 }
</script>
