<template>
  <div class="message message-success">
    <slot></slot>
  </div>
</template>

<style lang="sass">
 @import 'src/settings';
 @import 'src/mixins';

 .message-success {
   color: green;
   background-color: tint(green, 92%);
 }
</style>
