<template>
  <timesheet-list-view
      :entries="entries"
      :on-delete-request="deleteEntry"
      :on-update-request="updateEntry"
  ></timesheet-list-view>

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
 import Api from 'api'
 import TimesheetListView from './timesheetListView'
 import EntryForm from './entryForm'

 export default {
   data () {
     return {
       updatingEntry: undefined,
       disposable: new Rx.CompositeDisposable(),
       entries: []
     }
   },
   ready () {
     this.retrieve()
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
                 EntryForm }
 }
</script>
