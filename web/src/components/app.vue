<template>
  <div class="app">
    <router-view></router-view>
  </div>
</template>

<style lang="sass">
 @import 'src/settings';
 @import 'src/mixins';
 @import 'src/theme';

 .app {
   margin: 0 em(12);
   padding: em(56) 0;
   @include media($mobile) {
     padding: em(12) 0;
   }
 }
</style>
