<template>
  <timesheet-list-view
      :entries="entries"
  ></timesheet-list-view>

  <h2>New entry</h2>
  <entry-form></entry-form>
</template>

<script>
 import TimesheetListView from './timesheetListView'
 import EntryForm from './entryForm'

 export default {
   data () {
     return {
       entries: []
     }
   },
   attached () {
   },
   components: { TimesheetListView,
                 EntryForm }
 }
</script>
