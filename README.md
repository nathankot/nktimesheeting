# nktimesheeting

<http://nktimesheeting.herokuapp.com>

## The stack

### Static assets

* [Vue.js][vue]
* [RxJS][rxjs]
* [SASS][sass]

### API Server

* [Haskell][haskell]
* [Yesod][yesod]

## Dependencies

* [Stack][stack]
* NPM

## Development

Run a server for the static files on port `8080`:

    cd web
    npm install webpack -g
    npm install
    npm run dev

Run the API server on port `3000`:

    cd server
    stack build --exec server

The static server will automatically forward API requests to the API server
during development. Visit `http://localhost:8080/` to develop.

## Deployment

Use `make` to build everything:

    make

The above command requires the following binaries to be on your `PATH`:

* `docker`
* `stack`
* `npm`

Commit and push to Heroku:

    git commit -m "Build"
    git push heroku master

[vue]: http://vuejs.org
[sass]: http://sass-lang.com
[rxjs]: https://github.com/Reactive-Extensions/RxJS
[yesod]: https://github.com/yesodweb/yesod
[haskell]: http://haskell.org
[stack]: http://haskellstack.org
