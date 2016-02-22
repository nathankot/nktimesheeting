# nktimesheeting

<http://nktimesheeting.herokuapp.com>

## The stack

### Frontend

* [Vue.js][vue]
* [RxJS][rxjs]
* [SASS][sass]

### API Server

* [Haskell][haskell]
* [PostgreSQL][postgres]
* [Yesod][yesod]

## Toolchain dependencies

* [Stack][stack]
* NPM
* Docker

## Development

### Setup

Install postgres and make a new user and db:

    createuser newapp -W # choose 'newapp' for the password
    createdb newapp --owner=newapp
    createdb newapp-testing --owner=newapp

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

### Testing

Run server tests with `stack`

    cd server
    stack test

Run frontend tests with `npm`

    cd web
    npm test

## Deployment

Use `make` to build everything:

    make

Commit and push to Heroku:

    heroku buildpacks:set https://github.com/nathankot/heroku-binary-buildpack
    git commit -m "Build"
    git push heroku master

[vue]: http://vuejs.org
[sass]: http://sass-lang.com
[rxjs]: https://github.com/Reactive-Extensions/RxJS
[yesod]: https://github.com/yesodweb/yesod
[haskell]: http://haskell.org
[stack]: http://haskellstack.org
[postgres]: http://www.postgresql.org/
