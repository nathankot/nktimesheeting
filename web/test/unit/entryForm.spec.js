/* global describe, beforeEach, it, expect */

import Vue from 'vue'
import moment from 'moment'
import EntryForm from 'src/components/entryForm'

describe('EntryForm', () => {

  let vm

  beforeEach(() => {
    vm = new Vue({ template: '<div><entry-form v-ref:form></entry-form></div>',
                   components: { EntryForm } }).$mount()
  })

  it('creates a default entry with an earlier start date', () => {
    expect(moment(vm.$refs.form.$data.entry.start).toDate()).to.be.below(new Date())
  })

  it('creates a default end date later than start date', () => {
    expect(moment(vm.$refs.form.$data.entry.start).toDate())
      .to.be.below(moment(vm.$refs.form.$data.entry.end).toDate())
  })

  it('doesnt update the entry until save', (done) => {
    const textarea = vm.$el.querySelector('.field textarea')
    textarea.value = 'new value'
    textarea.dispatchEvent(new CustomEvent('onchange', { target: textarea }))
    vm.$nextTick(() => {
      expect(vm.$refs.form.$data.entry.note).to.be.empty
      done()
    })
  })

  it('can change the entry at any time', (done) => {
    const start = '1992-08-08T10:53:10.555Z'
    const end = '1992-08-08T16:53:08.543Z'

    vm.$refs.form.$data.entry = {
      id: 3,
      start: start,
      end: end,
      note: 'hello there'
    }

    vm.$nextTick(() => {
      expect(vm.$refs.form.$data.note).to.equal('hello there')
      expect(vm.$refs.form.$data.startDate).to.equal(moment(start).local().format('YYYY-MM-DD'))
      expect(vm.$refs.form.$data.endDate).to.equal(moment(end).local().format('YYYY-MM-DD'))
      done()
    })
  })

})
