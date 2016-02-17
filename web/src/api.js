import Vue from 'vue'
import Resource from 'vue-resource'
import Rx from 'rx'
import _ from 'underscore'

Vue.use(Resource)
Vue.resource.actions.update = { method: 'PATCH' }

Vue.Promise.prototype.rx = function () {
  return Rx.Observable.fromPromise(this)
}

export function getError (response) {
  if (_.isObject(response.data)) {
    if (_.isArray(response.data.errors) &&
        !_.isEmpty(response.data.errors)) {
      return _.first(response.data.errors)
    }

    if (_.isString(response.data.message)) {
      return response.data.message
    }
  }

  return 'Unknown error occured'
}

export default {
  users: Vue.resource('api/users{/id}'),
  sessions: Vue.resource('api/sessions')
}
