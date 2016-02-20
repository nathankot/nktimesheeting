<template>
  <div class="entries-list">
    <table>
      <thead>
        <tr>
          <th>Start</th>
          <th>End</th>
          <th>Duration</th>
          <th>Notes</th>
          <th v-if="editable"></th>
        </tr>
      </thead>
      <tbody>
        <tr class="entry" :class="{ 'highlight-red': isEntryPartOfBadDay(entry) }" v-for="entry in sortedEntries">
          <td>{{ moment.utc(entry.start).local().format(shortDateFormat) }}</td>
          <td>{{ moment.utc(entry.end).local().format(shortDateFormat) }}</td>
          <td>{{ moment.duration(moment(entry.end).diff(moment(entry.start))).format('h[h] m[m]') }}</td>
          <td>{{ entry.note }}</td>
          <td v-if="editable">
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
 import reduceEntriesToDays from 'src/filters/reduceEntriesToDays'

 export default {
   props: {
     onUpdateRequest: { type: Function, default () { return _.noop } },
     onDeleteRequest: { type: Function, default () { return _.noop } },
     entries: { required: true, type: Array },
     editable: { type: Boolean, default: true },
     preferredWorkingHours: { type: Number, default: 8 }
   },
   data () {
     return {
       _: _,
       moment: moment,
       shortDateFormat: shortDateFormat
     }
   },
   computed: {
     sortedEntries () {
       return _.sortBy(this.entries, e => moment(e.start).toDate())
     },

     days () {
       return reduceEntriesToDays(this.sortedEntries)
     }
   },
   methods: {
     isEntryPartOfBadDay (entry) {
       const date = moment.utc(entry.start).local().startOf('day')
       const day = _.find(this.days, (d) => moment(d.date).isSame(date))
       return moment.duration(day.seconds, 'seconds').asHours() < this.preferredWorkingHours
     }
   }
 }
</script>

<style lang="sass">
 tr.highlight-red {
   background-color: red;
 }
</style>
