const path = require('path')
const ExtractTextPlugin = require('extract-text-webpack-plugin')
const HtmlWebpackPlugin = require('html-webpack-plugin')
const CleanWebpackPlugin = require('clean-webpack-plugin')
const CopyWebpackPlugin = require('copy-webpack-plugin')
const noop = require('noop-webpack-plugin')
const webpack = require('webpack')

const IS_DEV = true

module.exports = {
  mode: IS_DEV ? 'development' : 'production',
  entry: {
    app: './src/index.js'
  },
  devtool: IS_DEV ? 'inline-source-map' : false,
  devServer: IS_DEV ? {
    contentBase: false,
    hot: true
  } : {},
  plugins: [
    new CleanWebpackPlugin(['dist']),
    new HtmlWebpackPlugin({
      title: 'Development'
    }),
    IS_DEV ? new webpack.HotModuleReplacementPlugin() : noop(),
    new webpack.DefinePlugin({
      IS_DEV: IS_DEV
    })
  ],
  output: {
    filename: 'bundle.js',
    path: path.resolve(__dirname, 'dist')
  },
  module: {
    rules: [
      !IS_DEV ? {
        test: /\.m?js$/,
        exclude: /(node_modules|bower_components)/,
        use: {
          loader: 'babel-loader',
          options: {
            presets: ['@babel/preset-env']
          }
        }
      } : {},
      {
        test: /\.css$/,
        use: [
          'style-loader',
          'css-loader',
          'postcss-loader'
        ]
      },
      {
        test: /\.html$/,
        use: [ {
          loader: 'html-loader',
          options: {
            minimize: true,
            removeComments: false,
            collapseWhitespace: false
          }
        }]
      },
      // the file-loader emits files.
      {
        test: /\.(woff(2)?|ttf|eot|svg)(\?v=[0-9]\.[0-9]\.[0-9])?$/,
        loader: 'file-loader'
      }
    ]
  },
  resolve: {
    mainFields: ['browser', 'module', 'main']
  }
}
