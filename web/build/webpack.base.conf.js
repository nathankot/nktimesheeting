var path = require('path')

module.exports = {
  entry: {
    app: './src/main.js'
  },
  output: {
    path: path.resolve(__dirname, '../dist/static'),
    publicPath: '/static/',
    filename: '[name].js'
  },
  resolve: {
    extensions: ['', '.js', '.vue'],
    alias: {
      'src': path.resolve(__dirname, '../src'),
      'rx': 'rx/dist/rx.lite.js',
      'api': path.resolve(__dirname, '../src/api.js'),
      'store': path.resolve(__dirname, '../src/store.js'),
      'config': path.resolve(__dirname, '../src/config.js')
    }
  },
  resolveLoader: {
    root: path.join(__dirname, 'node_modules')
  },
  module: {
    preLoaders: [
      {
        test: /\.vue$/,
        loader: 'eslint',
        exclude: /node_modules/
      },
      {
        test: /\.js$/,
        loader: 'eslint',
        exclude: /node_modules/
      }
    ],
    loaders: [
      {
        test: /\.vue$/,
        loader: 'vue'
      },
      {
        test: /\.js$/,
        loader: 'babel',
        exclude: /node_modules/
      },
      {
        test: /\.json$/,
        loader: 'json'
      },
      {
        test: /\.(png|jpg|gif|svg)$/,
        loader: 'url',
        query: {
          limit: 10000,
          name: '[name].[ext]?[hash:7]'
        }
      }
    ]
  },
  sassLoader: {
    includePaths: [
      path.resolve(__dirname, "../src/bourbon"),
      path.resolve(__dirname, "../src/neat"),
      path.resolve(__dirname, "../node_modules/normalize-scss/node_modules/support-for/sass"),
      path.resolve(__dirname, "../node_modules/normalize-scss/sass")
    ]
  },
  eslint: {
    formatter: require('eslint-friendly-formatter')
  }
}
