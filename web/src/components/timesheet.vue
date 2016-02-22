<template>
  <div class="timesheet">
    <menu class="filters">
      <div class="field-group of-three">
        <div class="field select-field">
          <div class="label">View entries for</div>
          <div class="select-wrapper">
            <select v-model="userId" @change="updateRoute('userId')($event)">
              <option :value="0">Myself</option>
              <option :value="user.id" v-for="user in viewableUsers">{{ user.email }}</option>
            </select>
          </div>
        </div>

        <div class="field right">
          <div class="label">From</div>
          <datepicker
              placeholder="Start date"
              format="YYYY-MM-DD"
              :on-change="updateRoute('start')"
              :value="start"
              required
          ></datepicker>
        </div>

        <div class="field right">
          <div class="label">To</div>
          <datepicker
              placeholder="End date"
              format="YYYY-MM-DD"
              :on-change="updateRoute('end')"
              :value="end"
              required
          ></datepicker>
        </div>
      </div>
    </menu>

    <router-view
        :entries="filteredEntries"
        :on-delete-request="deleteEntry"
        :on-update-request="updateEntry"
        :editable="editable"
        :preferred-working-hours="preferredWorkingHours"
    ></router-view>

    <menu class="export-options">
      <a class="button secondary small"
         :href="filteredEntries | reduceEntriesToDays | exportDaysToHTML"
         download="timesheet.html">
        Export HTML
      </a>
    </menu>

    <div class="entry-form">
      <h2 v-if="!updatingEntry && editable">New entry</h2>
      <entry-form :on-save="onNewEntry"
                  :user-id="userId"
                  v-if="!updatingEntry && editable"
      ></entry-form>

      <h2 v-if="updatingEntry">Edit entry</h2>
      <entry-form :on-save="onUpdatedEntry"
                  :on-cancel="onCancelUpdate"
                  :entry="updatingEntry"
                  v-if="updatingEntry"
      ></entry-form>
    </div>
  </div>
</template>

<script>
 import _ from 'underscore'
 import Rx from 'rx'
 import moment from 'moment'
 import Api from 'api'
 import Store from 'store'
 import reduceEntriesToDays from 'src/filters/reduceEntriesToDays'
 import exportDaysToHTML from 'src/filters/exportDaysToHTML'

 import TimesheetListView from './timesheetListView'
 import EntryForm from './entryForm'
 import Datepicker from './datepicker'

 export default {
   data () {
     return {
       userId: parseInt(this.$route.params.userId, 10),
       currentUser: null,
       updatingEntry: undefined,
       disposable: new Rx.CompositeDisposable(),
       entries: [],
       viewableUsers: [],
       preferredWorkingHours: 8
     }
   },

   computed: {
     start () {
       return this.$route.params.start
     },

     end () {
       return this.$route.params.end
     },

     editable () {
       return (
         (_.isObject(this.currentUser) && _.contains(this.currentUser.roles, 'Admin')) ||
         (_.isNumber(this.userId) && this.userId === 0))
     },

     filteredEntries () {
       return _.filter(this.entries, (e) => {
         return moment.utc(e.start).isAfter(moment(this.start).startOf('day')) &&
                moment.utc(e.end).isBefore(moment(this.end).endOf('day'))
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

     onCancelUpdate () {
       this.updatingEntry = undefined
     },

     updateEntry (entry) {
       this.updatingEntry = entry
       const $form = this.$el.querySelector('.entry-form')
       $form.scrollIntoView && $form.scrollIntoView()
     },

     deleteEntry (entry) {
       this.disposable.add(
         Api.entries.delete({ id: entry.id }).rx()
            .subscribeOnNext(() => {
              this.entries = _.filter(this.entries, (e) => e.id !== entry.id)
            }))
     },

     updateRoute (key) {
       // @param value - either an event or value
       return (value) => {
         if (_.isObject(value) && value.target && value.target.value) {
           value = value.target.value
         }
         var newParams = {}
         newParams[key] = value
         this.$route.router.go({
           name: this.$route.name,
           params: _.defaults(newParams, this.$route.params)
         })
       }
     },

     retrieve () {
       this.disposable.add(
         Api.entries
            .get({}, this.userId > 0 ? { userId: this.userId } : {})
            .rx()
            .subscribeOnNext((res) => {
              this.entries = res.data.entries
            }))
     }
   },

   watch: {
     userId (id) {
       this.retrieve()
     }
   },

   ready () {
     this.retrieve()

     this.disposable.add(
       Store.preferredWorkingHoursPerDay
         .subscribeOnNext(h => this.preferredWorkingHours = parseInt(h, 10)))

     this.disposable.add(
       Rx.Observable.combineLatest(
         Store.currentUser,
         Api.users.get().rx())
         .subscribeOnNext(({ 0: currentUser, 1: res }) => {
           this.currentUser = currentUser
           this.viewableUsers = _.isObject(currentUser)
             ? _.filter(res.data.users, (u) => u.id !== currentUser.id)
             : []
         }))
   },

   components: { TimesheetListView,
                 EntryForm,
                 Datepicker },

   filters: { reduceEntriesToDays,
              exportDaysToHTML }
 }
</script>

<style lang="sass">
 @import 'src/settings';
 @import 'src/mixins';

 .export-options {
   @include outer-container;
   text-align: right;
 }
</style>

<style lang="sass">
 @import 'src/settings';
 @import 'src/mixins';
 
 .timesheet {
   .filters {
     @include outer-container;
   }

   .timesheet-list {
     margin: 0 auto;
     max-width: $max-width * 1.25;
   }
 }

 .entry-form {
   @include outer-container;
 }

</style>
