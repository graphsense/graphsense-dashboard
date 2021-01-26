const path = require('path')
const HtmlWebpackPlugin = require('html-webpack-plugin')
const CleanWebpackPlugin = require('clean-webpack-plugin')
const webpack = require('webpack')
const MiniCssExtractPlugin = require('mini-css-extract-plugin')

module.exports = env => {
  const IS_DEV = !env || !env.production

  const output = {
    filename: '[name].js?[hash]',
    path: path.resolve(__dirname, 'dist')
  }

  const entry = {
    test: './test/test.js'
  }

  return {
    mode: 'development',
    entry,
    devtool: IS_DEV ? 'inline-source-map' : false,
    devServer: {
      contentBase: false,
      hot: true
    },
    plugins: [
      new CleanWebpackPlugin(['dist']),
      new HtmlWebpackPlugin({
        title: 'GraphSense App',
        favicon: './src/style/img/favicon.png'
      }),
      new webpack.HotModuleReplacementPlugin()
    ],
    output,
    module: {
      rules: [
        {
          test: /\[^(static)].m?js$/,
          exclude: /(node_modules|bower_components)/,
          use: [
            {
              loader: 'webpack-strip-block'
            },
            {
              loader: 'babel-loader',
              options: {
                presets: [
                  [
                    '@babel/preset-env',
                    {
                      targets: {
                        edge: '17',
                        firefox: '60',
                        chrome: '67',
                        safari: '11.1'
                      },
                      useBuiltIns: 'usage'
                    }
                  ]
                ]
              }
            }
          ]
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
