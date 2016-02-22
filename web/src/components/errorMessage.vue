<template>
  <div class="message message-error">
    <slot></slot>
  </div>
</template>

<style lang="sass">
 @import 'src/settings';
 @import 'src/mixins';

 .message {
   padding: em(16);
   border-radius: em(6);
   margin-bottom: em(12);
 }

 .message-error {
   color: red;
   background-color: tint(red, 92%);
 }
</style>
