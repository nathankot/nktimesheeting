<template>
  <div class="settings">
    <div class="field">
      <div class="label">Preferred working hours per day</div>
      <input type="number" v-model="preferredWorkingHours" />
    </div>
  </div>
</template>

<script>
 import Store from 'store'
 import Rx from 'rx'

 export default {
   data () {
     return {
       disposable: new Rx.CompositeDisposable(),
       preferredWorkingHours: 8
     }
   },

   watch: {
     preferredWorkingHours (h) {
       Store.preferredWorkingHoursPerDay.onNext(h)
     }
   },

   ready () {
     this.disposable.add(
       Store.preferredWorkingHoursPerDay
            .take(1)
            .subscribeOnNext(h => this.preferredWorkingHours = h))
   }
 }
</script>
