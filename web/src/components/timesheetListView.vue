<template>
  <div class="entries-list">
    <table>
      <thead>
        <tr>
          <th>Start</th>
          <th>End</th>
          <th>Notes</th>
          <th></th>
        </tr>
      </thead>
      <tbody>
        <tr class="entry" v-for="entry in entries">
          <td>{{ moment.utc(entry.start).format(shortDateFormat) }}</td>
          <td>{{ moment.utc(entry.end).format(shortDateFormat) }}</td>
          <td>{{ entry.note }}</td>
          <td>
            <a @click="onUpdateRequest(entry)">Edit</a>
            <a @click="onDeleteRequest(entry)">Remove</a>
          </td>
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
     onUpdateRequest: { type: Function, default () { return _.noop } },
     onDeleteRequest: { type: Function, default () { return _.noop } },
     entries: { required: true, type: Array }
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
