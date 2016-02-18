/* global encodeURIComponent */

import _ from 'underscore'
import moment from 'moment'
import 'moment-duration-format'
import escape from './escape'

// @returns data URI of the downloadable HTML table
export default function (days) {

  var content = _.map(days, function ({ date, seconds, notes }) {
    const dateString = moment(date).format('YYYY-MM-DD')
    const durationString = moment.duration(seconds, 'seconds').format('h[h] m[m]')
    const notesString = _.chain(notes).map(escape).join('<br>').value()

    return `<tr>
              <td>${dateString}</td>
              <td>${durationString}</td>
              <td>${notesString}</td>
            </tr>`
  })

  var html = `
    <html>
      <body>
        <table>
          <tr>
            <th>Date</th>
            <th>Duration</th>
            <th>Notes</th>
          </tr>
          ${content}
        </table>
      </body>
    </html>
  `

  return 'data:text/html,' + encodeURIComponent(html)

}

