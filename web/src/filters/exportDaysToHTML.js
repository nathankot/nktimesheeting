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
              <td align="center">${dateString}</td>
              <td align="center">${durationString}</td>
              <td>${notesString}</td>
            </tr>`
  })

  var html = `
    <html>
      <head>
        <style>
          body {
            font-family: Helvetica, Verdana, sans-serif;
            font-size: 16px;
          }

          td, th {
            vertical-align: top;
          }
        </style>
      </head>
      <body>
        <table width="100%">
          <tr>
            <th width="160" align="center">Date</th>
            <th width="160" align="center">Duration</th>
            <th>Notes</th>
          </tr>
          ${content.join('\n')}
        </table>
      </body>
    </html>
  `

  return 'data:text/html,' + encodeURIComponent(html)

}

