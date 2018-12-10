const path = require('path')
const HtmlWebpackPlugin = require('html-webpack-plugin')
const CleanWebpackPlugin = require('clean-webpack-plugin')
const noop = require('noop-webpack-plugin')
const webpack = require('webpack')

module.exports = env => {
  let IS_DEV = !env || !env.production
  console.log(IS_DEV ? 'Development mode' : 'Production mode')
  return {
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
        IS_DEV: IS_DEV,
        REST_ENDPOINT: !IS_DEV ? '\'{{REST_ENDPOINT}}\'' : '\'http://localhost:9000\''
      }),
      new webpack.ProvidePlugin({
        $: 'jquery',
        jQuery: 'jquery'
      })
    ],
    output: {
      filename: 'bundle.js',
      path: path.resolve(__dirname, 'dist')
    },
    module: {
      rules: [
        {
          test: /\.m?js$/,
          exclude: /(node_modules|bower_components)/,
          use: [
            {
              loader: 'babel-loader',
              options: {
                presets: ['@babel/preset-env'],
                plugins: ['@babel/plugin-proposal-optional-chaining']
              }
            }
          ]
        },
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
          test: /\.(woff(2)?|ttf|eot|svg|jpg|png)(\?v=[0-9]\.[0-9]\.[0-9])?$/,
          loader: 'file-loader'
        }
      ]
    },
    resolve: {
      mainFields: ['browser', 'module', 'main']
    }
  }
}
