import Vue from 'vue'
import Router from 'vue-router'
import Resource from 'vue-resource'
import App from './components/app'
import Hello from './components/app'

Vue.use(Router)
Vue.use(Resource)

const router = new Router()

router.beforeEach(() => window.scrollTo(0, 0))
router.redirect({ '*': '/' })
router.map({
  '/': { component: Hello }
})

router.start(App, '#app')
