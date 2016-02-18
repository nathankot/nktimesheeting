<template>

  <menu class="date-filter">
    <div class="field">
      <div class="label">From</div>
      <datepicker
          placeholder="Start date"
          :value.sync="startDate"
          format="YYYY-MM-DD"
          required
      ></datepicker>
    </div>

    <div class="field">
      <div class="label">To</div>
      <datepicker
          placeholder="End date"
          :value.sync="endDate"
          format="YYYY-MM-DD"
          required
      ></datepicker>
    </div>
  </menu>

  <timesheet-list-view
      :entries="filteredEntries"
      :on-delete-request="deleteEntry"
      :on-update-request="updateEntry"
  ></timesheet-list-view>

  <menu class="export-options">
    <a :href="filteredEntries | reduceEntriesToDays | exportDaysToHTML"
       download="timesheet.html">
      Export HTML
    </a>
  </menu>

  <h2 v-if="!updatingEntry">New entry</h2>
  <entry-form :on-save="onNewEntry"
              v-if="!updatingEntry"></entry-form>

  <h2 v-if="updatingEntry">Edit entry</h2>
  <entry-form :on-save="onUpdatedEntry"
              :entry="updatingEntry"
              v-if="updatingEntry"></entry-form>
</template>

<script>
 import _ from 'underscore'
 import Rx from 'rx'
 import moment from 'moment'
 import Api from 'api'
 import TimesheetListView from './timesheetListView'
 import EntryForm from './entryForm'
 import Datepicker from './datepicker'
 import reduceEntriesToDays from 'src/filters/reduceEntriesToDays'
 import exportDaysToHTML from 'src/filters/exportDaysToHTML'

 export default {
   data () {
     return {
       updatingEntry: undefined,
       disposable: new Rx.CompositeDisposable(),
       entries: [],
       startDate: moment().startOf('day').format('YYYY-MM-DD'),
       endDate: moment().endOf('day').format('YYYY-MM-DD')
     }
   },
   computed: {
     filteredEntries () {
       return _.filter(this.entries, (e) => {
         return moment.utc(e.start).isAfter(moment(this.startDate).startOf('day')) &&
                moment.utc(e.end).isBefore(moment(this.endDate).endOf('day'))
       })
     }
   },
   methods: {
     onNewEntry (entry) {
       this.entries.push(entry)
     },

     onUpdatedEntry (entry) {
       this.updatingEntry = undefined
     },

     updateEntry (entry) {
       this.updatingEntry = entry
     },

     deleteEntry (entry) {
       this.disposable.add(
         Api.entries.delete({ id: entry.id }).rx()
            .subscribeOnNext(() => {
              this.entries = _.filter(this.entries, (e) => e.id !== entry.id)
            }))
     },

     retrieve () {
       this.disposable.add(
         Api.entries.get().rx()
            .subscribeOnNext((res) => {
              this.entries = res.data.entries
            }))
     }
   },
   ready () {
     this.retrieve()
   },
   components: { TimesheetListView,
                 EntryForm,
                 Datepicker },
   filters: { reduceEntriesToDays,
              exportDaysToHTML }
 }
</script>
