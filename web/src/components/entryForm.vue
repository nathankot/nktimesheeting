<template>
  <error-message v-show="error">{{ error }}</error-message>

  <div class="field-group">
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

  <div class="field-group">
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

  <div class="field">
    <div class="label">Notes</div>
    <textarea v-model="entry.note" rows="10"></textarea>
  </div>

  <button class="submit" @click="submit">
    {{ isCreate ? 'Create' : 'Update' }}
  </button>
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
     start: moment().subtract(1, 'hour').utc().format(dateFormat),
     end: moment.utc().format(dateFormat),
     note: ''
   }
 }

 export default {
   props: {
     onSave: { type: Function, default () { return _.constant } },
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
       disposable: new Rx.CompositeDisposable(),
       startDate: moment.utc(this.entry.start).local().format('YYYY-MM-DD'),
       startTime: moment.utc(this.entry.start).local().format('HH:mm'),
       endDate: moment.utc(this.entry.end).local().format('YYYY-MM-DD'),
       endTime: moment.utc(this.entry.end).local().format('HH:mm')
     }
   },
   computed: {
     isCreate () { return !_.isNumber(this.entry.id) }
   },
   watch: {
     startDate: 'updateEntryDates',
     startTime: 'updateEntryDates',
     endDate: 'updateEntryDates',
     endTime: 'updateEntryDates',
     userId: function (userId) {
       this.entry.userId = userId
     }
   },
   methods: {
     updateEntryDates () {
       var start = moment(this.startDate + ' ' + this.startTime)
       var end = moment(this.endDate + ' ' + this.endTime)
       if (end.isBefore(start)) {
         end = start.add(1, 'hour')
         this.endDate = end.format('YYYY-MM-DD')
         this.endTime = end.format('HH:mm')
       }
       this.entry.start = start.utc().format(dateFormat)
       this.entry.end = end.utc().format(dateFormat)
     },

     submit () {
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
     }
   },
   beforeDestroy () {
     this.disposable.dispose()
   },
   components: { Datepicker,
                 ErrorMessage }
 }
</script>
