<template>
  <div class="message message-error">
    <slot></slot>
  </div>
</template>

<style lang="sass">
 @import 'src/settings';
 @import 'src/mixins';

 .message-error {
   color: red;
   background-color: tint(red, 92%);
 }
</style>
