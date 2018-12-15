const path = require('path')
const HtmlWebpackPlugin = require('html-webpack-plugin')
const CleanWebpackPlugin = require('clean-webpack-plugin')
const noop = require('noop-webpack-plugin')
const webpack = require('webpack')
const StaticSiteGeneratorPlugin = require('static-site-generator-webpack-plugin')
const hb = require('handlebars')
const fs = require('fs')
const MiniCssExtractPlugin = require('mini-css-extract-plugin')

const VERSION = '0.4'

// compose pre-rendered landing page
let template = hb.compile(fs.readFileSync('./src/pages/page.hbs', 'utf-8'))
let landingpage = fs.readFileSync('./src/pages/landingpage.html', 'utf-8')
let footer = hb.compile(fs.readFileSync('./src/pages/footer.html', 'utf-8'))
footer = footer({version: VERSION})

const DEV_REST_ENDPOINT = 'http://localhost:9000'

module.exports = env => {
  let IS_DEV = !env || !env.production

  let output = {
    filename: '[name].js',
    path: path.resolve(__dirname, 'dist')
  }

  if (!IS_DEV) {
    output['libraryTarget'] = 'umd' // needed for static-site-generator-plugin
    output['globalObject'] = 'this' // fix issue with webpack 4, see https://github.com/markdalgleish/static-site-generator-webpack-plugin/issues/130
  }

  console.log(IS_DEV ? 'Development mode' : 'Production mode')
  return {
    mode: IS_DEV ? 'development' : 'production',
    entry: {
      static: './src/static.js',
      main: './src/index.js'
    },
    devtool: IS_DEV ? 'inline-source-map' : false,
    devServer: IS_DEV ? {
      contentBase: false,
      hot: true
    } : {},
    plugins: [
      new CleanWebpackPlugin(['dist']),
      new HtmlWebpackPlugin({
        title: 'GraphSense App',
        excludeChunks: ['static'],
        template: './src/pages/page.hbs',
        page: landingpage,
        footer: footer,
        main: 'main'
      }),
      IS_DEV ? new webpack.HotModuleReplacementPlugin() : noop(),
      new webpack.DefinePlugin({
        IS_DEV: IS_DEV,
        REST_ENDPOINT: !IS_DEV ? '\'{{REST_ENDPOINT}}\'' : '\'' + DEV_REST_ENDPOINT + '\'',
        VERSION: '\'' + VERSION + '\''
      }),
      new webpack.ProvidePlugin({
        $: 'jquery',
        jQuery: 'jquery'
      }),
      !IS_DEV ? new StaticSiteGeneratorPlugin({
        paths: [
          '/terms.html',
          '/privacy.html',
          '/about.html'
        ],
        entry: 'static', // refers to entry.static
        locals: {
          template: template,
          footer: footer,
          main: 'main',
          header: true
        }
      }) : noop(),
      new MiniCssExtractPlugin({
        filename: '[name].css',
        chunkFilename: '[id].css'
      })
    ],
    output: output,
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
            MiniCssExtractPlugin.loader,
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
          test: /\.(woff(2)?|ttf|eot|svg|jpg|png|gif)(\?v=[0-9]\.[0-9]\.[0-9])?$/,
          loader: 'file-loader'
        },
        {
          test: /\.hbs$/,
          loader: 'handlebars-loader'
        }
      ]
    },
    resolve: {
      mainFields: ['browser', 'module', 'main']
    }
  }
}
