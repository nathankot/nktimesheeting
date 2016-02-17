<template>
  <div class="entries-list">
    <table>
      <thead>
        <tr>
          <th>Start</th>
          <th>End</th>
          <th>Notes</th>
        </tr>
      </thead>
      <tbody>
        <tr class="entry" v-for="entry in entries">
          <td>{{ moment.utc(entry.start).format(shortDateFormat) }}</td>
          <td>{{ moment.utc(entry.end).format(shortDateFormat) }}</td>
          <td>{{ entry.note }}</td>
        </tr>
      </tbody>
    </table>
  </div>
</template>

<script>
 import moment from 'moment'
 import _ from 'underscore'
 import { shortDateFormat } from 'config'

 export default {
   props: {
     entries: { required: true, type: Array, default () { return [] } }
   },
   data () {
     return {
       _: _,
       moment: moment,
       shortDateFormat: shortDateFormat
     }
   }
 }
</script>
