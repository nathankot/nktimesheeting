/* global describe, it, encodeURIComponent, expect  */

import moment from 'moment'
import exportDaysToHTML from 'src/filters/exportDaysToHTML'

describe('exportDaysToHTML', () => {

  var fixture = [
    { date: moment('2016-02-17').toDate(),
      seconds: 3600,
      notes: ['one', 'two', 'three'] },

    { date: moment('2016-02-18').toDate(),
      seconds: 36565,
      notes: ['<script>window.alert("ha")</script>', 'five', 'six'] }
  ]

  it('builds the rows correctly', () => {
    var result = exportDaysToHTML(fixture)
    expect(result).to.contain(encodeURIComponent('10h 9m'))
    expect(result).to.contain(encodeURIComponent('1h 0m'))
  })

  it('escapes any user input', () => {
    var result = exportDaysToHTML(fixture)
    expect(result).to.contain(encodeURIComponent('script'))
    expect(result).to.not.contain(encodeURIComponent('<script>'))
  })

})
