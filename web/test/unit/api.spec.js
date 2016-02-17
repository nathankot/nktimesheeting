/* global describe, it, expect */

import 'api'
import Store from 'store'
import Vue from 'vue'

describe('Api', () => {

  it('sets the authorization header when given a new user', () => {
    Store.currentUser.onNext({
      email: 'testing@test.com',
      apiKey: { value: 'notarealapikey123' }
    })

    expect(Vue.http.headers.common['Authorization'])
      .to.equal('Bearer notarealapikey123')
  })

  it('removes the authorization header when logging out', () => {
    Store.currentUser.onNext({
      email: 'testing@test.com',
      apiKey: { value: 'notarealapikey123' }
    })
    expect(Vue.http.headers.common['Authorization']).to.not.be.empty
    Store.currentUser.onNext(null)
    expect(Vue.http.headers.common['Authorization']).to.be.empty
  })

})
