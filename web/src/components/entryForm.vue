<template>
  <error-message v-show="error">{{ error }}</error-message>

  <form class="entry-form" @submit.prevent="submit">
    <div class="date-times">
      <div class="field-group of-two">
        <div class="field">
          <div class="label">Start date</div>
          <datepicker
              placeholder="Start date"
              :value.sync="startDate"
              format="YYYY-MM-DD"
              required
          ></datepicker>
        </div>

        <div class="field">
          <div class="label">Start time</div>
          <input type="time" v-model="startTime" required />
        </div>
      </div>

      <div class="field-group of-two">
        <div class="field">
          <div class="label">End date</div>
          <datepicker
              placeholder="End date"
              :value.sync="endDate"
              format="YYYY-MM-DD"
              required
          ></datepicker>
        </div>

        <div class="field">
          <div class="label">End time</div>
          <input type="time" v-model="endTime" required />
        </div>
      </div>
    </div>

    <div class="notes-and-submit">
      <div class="field">
        <div class="label">Notes</div>
        <textarea v-model="note" rows="10"></textarea>
      </div>

      <input class="submit"
            type="submit"
            :value="isCreate ? 'Create' : 'Update'" />

      <a class="button secondary"
         v-if="allowCancel"
         @click="cancel">
        Cancel
      </a>
    </div>
  </form>

</template>

<style lang="sass">
</style>

<script>
 import moment from 'moment'
 import { dateFormat } from 'config'
 import Api, { getError } from 'api'
 import Rx from 'rx'
 import _ from 'underscore'

 import Datepicker from './datepicker'
 import ErrorMessage from './errorMessage'

 function defaultEntry () {
   return {
     id: null,
     userId: this.userId || undefined,
     start: moment().subtract(1, 'hour').startOf('hour').utc().format(dateFormat),
     end: moment().startOf('hour').utc().format(dateFormat),
     note: ''
   }
 }

 export default {
   props: {
     onSave: { type: Function, default: _.noop },
     onCancel: { type: Function, default: _.noop },
     userId: {},
     entry: {
       type: Object,
       default () { return defaultEntry.apply(this) }
     }
   },

   data () {
     return {
       _: _,
       error: null,
       note: '',
       disposable: new Rx.CompositeDisposable(),
       startDate: moment().utc().format('YYYY-MM-DD'),
       startTime: moment().utc().format('HH:mm'),
       endDate: moment().utc().format('YYYY-MM-DD'),
       endTime: moment().utc().format('HH:mm')
     }
   },

   computed: {
     isCreate () { return !_.isNumber(this.entry.id) },
     allowCancel () { return this.onCancel !== _.noop }
   },

   watch: {
     userId: function (userId) {
       if (_.isNumber(userId) && userId > 0) {
         this.entry.userId = userId
       } else {
         delete this.entry.userId
       }
     },

     entry (entry) {
       this.updateFromEntry(entry)
     }
   },

   methods: {
     updateFromEntry (entry) {
       this.note = entry.note
       this.startDate = moment.utc(entry.start).local().format('YYYY-MM-DD')
       this.startTime = moment.utc(entry.start).local().format('HH:mm')
       this.endDate = moment.utc(entry.end).local().format('YYYY-MM-DD')
       this.endTime = moment.utc(entry.end).local().format('HH:mm')
     },

     submit () {
       var start = moment(this.startDate + ' ' + this.startTime)
       var end = moment(this.endDate + ' ' + this.endTime)
       this.entry.start = start.utc().format(dateFormat)
       this.entry.end = end.utc().format(dateFormat)
       this.entry.note = this.note

       this.error = null
       this.disposable.add(
         (this.isCreate
        ? Api.entries.save(this.entry)
        : Api.entries.update({ id: this.entry.id }, this.entry))
              .rx()
              .doOnNext((r) => {
                this.onSave(r.data.entry)
                this.entry = defaultEntry.apply(this)
              })
              .subscribeOnError((r) => this.error = getError(r)))
     },

     cancel () {
       this.onCancel()
     }
   },

   ready () {
     this.updateFromEntry(this.entry)
   },

   beforeDestroy () {
     this.disposable.dispose()
   },

   components: { Datepicker,
                 ErrorMessage }
 }
</script>

<style lang="sass">
 @import 'src/settings';
 @import 'src/mixins';

 .entry-form {
   .date-times {
     @include span-columns(8);
     @include media($mobile) {
       @include span-columns(6);
     }
   }

   .notes-and-submit {
     @include span-columns(12);
     @include clearfix;
   }
 }
</style>
