<template>
  <timesheet-list-view
      :entries="entries"
  ></timesheet-list-view>

  <h2>New entry</h2>
  <entry-form :on-save="onNewEntry"></entry-form>
</template>

<script>
 import Rx from 'rx'
 import Api from 'api'
 import TimesheetListView from './timesheetListView'
 import EntryForm from './entryForm'

 export default {
   data () {
     return {
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

     retrieve () {
       this.disposable.add(
         Api.entries.get().rx()
            .subscribeOnNext((res) => {
              this.entries = res.data.entries
            }))
     }
   },
   components: { TimesheetListView,
                 EntryForm }
 }
</script>
