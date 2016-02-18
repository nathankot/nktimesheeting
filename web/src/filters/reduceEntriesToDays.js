import _ from 'underscore'
import moment from 'moment'

export default function (entries) {

  // Convert entries into tallies for their respective dates
  return _.chain(entries)
    .sortBy((e) => moment(e.start).toDate())
    .reduce(function (days, entry) {
      var start = moment(entry.start)
      var end = moment(entry.end)

      var dayStart = start.clone().startOf('day')
      // Sometimes entries will span > 1 day
      while (end.isAfter(dayStart)) {
        var dayIden = dayStart.format('YYYY-MM-DD')
        var dayEnd = dayStart.clone().endOf('day')

        if (!_.isObject(days[dayIden])) {
          days[dayIden] = {
            date: dayStart.clone().toDate(),
            seconds: 0,
            notes: []
          }
        }

        if (!_.isEmpty(entry.note)) {
          days[dayIden].notes.push(entry.note)
        }

        days[dayIden].seconds += moment.duration(
          moment.min(end, dayEnd)
            .diff(moment.max(start, dayStart)))
          .asSeconds()

        dayStart.add(1, 'day')
      }

      return days
    }, {})
    .pairs()
    .map(([_, d]) => d)
    .sortBy('date')
    .value()

}
