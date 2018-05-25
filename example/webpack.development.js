const path = require('path')
const Webpack = require('webpack')
const HtmlWebpackPlugin = require('html-webpack-plugin')

const resolve = (...paths) => path.resolve(__dirname, ...paths)

module.exports = {
  target: 'web',
  mode: 'development',
  resolve: {
    extensions: ['.js', '.json'],
    modules: [
      resolve('src'),
      resolve('..', 'src'),
      resolve('node_modules'),
      resolve('..', 'node_modules'),
    ],
  },
  watch: true,
  entry: [
    resolve('src', 'index.bs.js')
  ],
  devtool: 'source-map',
  output: {
    filename: '[name].js',
    publicPath: '/',
  },
  performance: {
    hints: false,
  },
  plugins: [
    new Webpack.LoaderOptionsPlugin({
      minimize: true,
      debug: true,
    }),
    new Webpack.DefinePlugin({
      'process.env.NODE_ENV': JSON.stringify('development'),
    }),
    new HtmlWebpackPlugin({
      title: 're-active example',
      template: resolve('index.ejs'),
    }),
  ],
}
