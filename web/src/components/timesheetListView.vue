<template>
  <div class="timesheet-list">
    <table class="data-table">
      <thead>
        <tr>
          <th class="small">Start</th>
          <th class="small">End</th>
          <th class="small">Duration</th>
          <th>Notes</th>
          <th class="small" v-if="editable"></th>
        </tr>
      </thead>
      <tbody>
        <tr class="loading-message" v-if="loading && sortedEntries.length == 0">
          <td colspan="5">
            <h3>Loading...</h3>
          </td>
        </tr>

        <tr class="no-entries-message" v-if="!loading && sortedEntries.length == 0">
          <td colspan="5">
            <h3>There are no entries between these dates.</h3>
          </td>
        </tr>

        <tr class="entry" :class="{ 'highlight-red': isEntryPartOfBadDay(entry) }" v-for="entry in sortedEntries">
          <td>{{ moment.utc(entry.start).local().format(shortDateFormat) }}</td>
          <td>{{ moment.utc(entry.end).local().format(shortDateFormat) }}</td>
          <td>{{ moment.duration(moment(entry.end).diff(moment(entry.start))).format('h[h] m[m]') }}</td>
          <td class="note-column">{{ entry.note }}</td>
          <td v-if="editable">
            <menu class="row-action">
              <ul>
                <li>
                  <a @click="onUpdateRequest(entry)">Edit</a>
                </li>
                <li>
                  <a @click="onDeleteRequest(entry)">Remove</a>
                </li>
              </ul>
            </menu>
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
       loading: true,
       moment: moment,
       shortDateFormat: shortDateFormat
     }
   },
   watch: {
     entries () {
       this.loading = false
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
 @import 'src/settings';
 @import 'src/mixins';

 .timesheet-list {
   th.small {
     width: em(200);
   }

   tr.highlight-red {
     background-color: tint(pink, 50%);
     td {
       border-bottom: 1px dashed white;
       border-top: 1px dashed white;
     }
   }
   
   .note-column {
     text-align: left;
   }
 }
</style>
