<template>
  <user-header></user-header>
  <router-view></router-view>
</template>

<script>
 import UserHeader from './userHeader'
 export default {
   components: { UserHeader }
 }
</script>
