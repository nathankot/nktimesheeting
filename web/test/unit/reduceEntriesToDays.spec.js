/* global describe, it, expect, beforeEach, afterEach */

import reduceEntriesToDays from 'src/filters/reduceEntriesToDays'
import moment from 'moment'
import _ from 'underscore'

describe('reduceEntriesToDays', () => {

  // Fixtures have no UTC offsets so that testing is consistent
  // on diferent timezones.
  const entriesFixture = [
    { note: 'note one', start: '2016-02-17 20:23:14.644',
      userId: 1, end: '2016-02-18 04:16:14.649', id: 1 },

    { note: 'note two', start: '2016-02-19 20:26:23.184',
      userId: 1, end: '2016-02-19 21:26:23.184', id: 2 },

    { note: 'note two', start: '2016-02-22 20:26:23.184',
      userId: 1, end: '2016-02-23 21:26:23.184', id: 3 },

    { note: 'note two', start: '2016-02-23 20:26:23.184',
      userId: 1, end: '2016-02-23 21:26:23.184', id: 4 } ]

  var originalUtcOffset = moment().utcOffset()

  beforeEach(() => {
    moment().utcOffset(0)
  })

  afterEach(() => {
    moment().utcOffset(originalUtcOffset)
  })

  it('works on an empty list of entries', () => {
    var result = reduceEntriesToDays([])
    expect(_.isEmpty(result)).to.be.true
  })

  it('works with entries spanning multiple days', () => {
    var result = reduceEntriesToDays(entriesFixture)
    expect(resultToHours(result[0])).to.equal(4)
    expect(resultToHours(result[1])).to.equal(4)
    expect(resultToHours(result[2])).to.equal(1)
  })

  it('repeats notes on entries over multiple days', () => {
    var result = reduceEntriesToDays(entriesFixture)
    expect(result[3].notes.length).to.equal(1)
    expect(result[4].notes.length).to.equal(2)
  })

})

function resultToHours (result) {
  return Math.round(moment.duration(result.seconds, 'seconds').asHours())
}
