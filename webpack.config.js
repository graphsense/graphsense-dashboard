const path = require('path')
const glob = require('glob-all')
const HtmlWebpackPlugin = require('html-webpack-plugin')
const CleanWebpackPlugin = require('clean-webpack-plugin')
const noop = require('noop-webpack-plugin')
const webpack = require('webpack')
const MiniCssExtractPlugin = require('mini-css-extract-plugin')
const PurgecssPlugin = require('purgecss-webpack-plugin')
const CopyWebpackPlugin = require('copy-webpack-plugin')
const MomentTimezoneDataPlugin = require('moment-timezone-data-webpack-plugin')
const CompressionPlugin = require('compression-webpack-plugin')

const VERSION = '0.5.1'
const DEV_REST_ENDPOINT = 'https://api.graphsense.info/'

const src = path.join(__dirname, 'src')

module.exports = env => {
  const IS_DEV = !env || !env.production

  const output = {
    filename: '[name].js?[hash]',
    path: path.resolve(__dirname, 'dist')
  }

  const entry = {
    main: './src/index.js',
    sw: './src/sw.js'
  }

  output.globalObject = 'self'

  console.log(IS_DEV ? 'Development mode' : 'Production mode')
  return {
    mode: IS_DEV ? 'development' : 'production',
    entry,
    devtool: IS_DEV ? 'inline-source-map' : false,
    devServer: IS_DEV ? {
      contentBase: false,
      hot: true,
      host: '0.0.0.0'
    } : {},
    plugins: [
      new CleanWebpackPlugin(['dist']),
      new HtmlWebpackPlugin({
        title: 'GraphSense App',
        favicon: './src/style/img/favicon.png'
      }),
      new CopyWebpackPlugin({
        patterns: [
          { from: './config', to: './config/' },
          { from: './lang', to: './lang/' }
        ]
      }),
      IS_DEV ? new webpack.HotModuleReplacementPlugin() : noop(),
      new webpack.DefinePlugin({
        IS_DEV: IS_DEV,
        IMPORT_APP: IS_DEV ? 'import Model from "./app.js"' : '',
        REST_ENDPOINT: !IS_DEV ? '\'{{REST_ENDPOINT}}\'' : '\'' + DEV_REST_ENDPOINT + '\'',
        VERSION: '\'' + VERSION + '\''
      }),
      new webpack.ProvidePlugin({
        $: 'jquery',
        jQuery: 'jquery'
      }),
      new MiniCssExtractPlugin({
        filename: '[name].css?[hash]',
        chunkFilename: '[id].css'
      }),
      !IS_DEV ? new PurgecssPlugin({
        paths: glob.sync([
          path.join(src, '**', '*.js'),
          path.join(src, '**', '*.html'),
          path.join(src, '**', '*.hbs')
        ], { nodir: true }),
        extractors: [
          {
            extractor: class {
              static extract (content) {
                return content.match(/[A-Za-z0-9-_:\/]+/g) || []
              }
            },
            extensions: ['html', 'js', 'hbs']
          }
        ],
        whitelistPatterns: [
          /d3-context-menu.+/,
          /svg.+/
        ],
        whitelistPatternsChildren: [
          /d3-context-menu.+/,
          /DTS/,
          /dataTables/,
          /dataTable/,
          /fa-.+/,
          /min-h-full/,
          /svg.+/
        ]
      }) : noop(),
      new MomentTimezoneDataPlugin({
        startYear: 2009,
        endYear: 2030
      }),
      new CompressionPlugin()
    ],
    output,
    module: {
      rules: [
        {
          test: /\.m?js$/,
          exclude: /(node_modules|bower_components)/,
          use: (!IS_DEV ? [{ loader: 'webpack-strip-block' }] : []).concat(
            [{
              loader: 'babel-loader',
              options: {
                presets: [
                  [
                    '@babel/preset-env'
                  ]
                ]
              }
            }])
        },
        {
          test: /\.css$/,
          use: [
            MiniCssExtractPlugin.loader,
            { loader: 'css-loader', options: { importLoaders: 1 } },
            'postcss-loader'
          ]
        },
        {
          test: /\.html$/,
          use: [{
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
          test: /\.(woff(2)?|ttf|eot|svg|jpe?g|png|gif)(\?v=[0-9]\.[0-9]\.[0-9])?$/,
          loader: 'file-loader'
        }
      ]
    },
    resolve: {
      mainFields: ['browser', 'module', 'main']
    }
  }
}
