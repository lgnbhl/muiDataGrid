const webpack = require('webpack');
const path = require('path');
const { LicenseWebpackPlugin } = require('license-webpack-plugin');

const config = {
  entry: './src/index.js',
  mode: 'production',
  output: {
    path: path.join(__dirname, '..', 'inst', 'www', 'muiDataGrid'),
    filename: 'x-data-grid.js',
  },
  resolve: { extensions: ['.js', '.jsx', '.ts', '.tsx'] },
  module: {
    rules: [
      {
        test: /\.(js|jsx|ts|tsx)$/,
        exclude: /node_modules/,
        use: ['babel-loader'],
      },
      {
        test: /\.css$/,
        use: ['style-loader', 'css-loader'],
      },
    ],
  },
  externals: {
    'react': 'jsmodule["react"]',
    'react-dom': 'jsmodule["react-dom"]',
    '@/shiny.react': 'jsmodule["@/shiny.react"]',
    '@/muiMaterial': 'jsmodule["@/muiMaterial"]',
    '@mui/material': 'jsmodule["@mui/material"]',
    '@mui/system': 'jsmodule["@mui/system"]',
    '@mui/utils':  'jsmodule["@mui/utils"]',
    // Share muiMaterial's single emotion instance so the grid reads the same
    // ThemeContext (otherwise ThemeProvider/sx styling is ignored). styled is a
    // default import (must be the function); react is consumed via named
    // imports (must be the namespace). The `.default || self` guard makes styled
    // work whether muiMaterial exposes the function or a {default} namespace.
    '@emotion/react': 'jsmodule["@emotion/react"]',
    '@emotion/styled': '(jsmodule["@emotion/styled"].default || jsmodule["@emotion/styled"])'
  },
  plugins: [
    new webpack.DefinePlugin({ 'process.env': '{}' }),
    new LicenseWebpackPlugin({
      outputFilename: 'x-data-grid.js.LICENSE.txt',
      // Only the 'MUI X Data Grid' family is bundled into x-data-grid.js. The
      // runtime externals below (react, react-dom, @mui/material, @mui/system,
      // @mui/utils, @emotion/*) are provided by 'shiny.react' and 'muiMaterial'
      // and are not redistributed, so their declared transitive deps (e.g.
      // react-transition-group) are excluded from the attribution file.
      excludedPackageTest: (name) =>
        [
          'react',
          'react-dom',
          'react-transition-group',
          '@mui/material',
          '@mui/system',
          '@mui/utils',
          '@emotion/react',
          '@emotion/styled',
        ].includes(name),
      additionalModules: [
        { name: '@mui/x-data-grid', directory: path.resolve(__dirname, 'node_modules/@mui/x-data-grid') },
        { name: '@mui/x-virtualizer', directory: path.resolve(__dirname, 'node_modules/@mui/x-virtualizer') },
        { name: '@mui/x-internals', directory: path.resolve(__dirname, 'node_modules/@mui/x-internals') },
        { name: '@mui/private-theming', directory: path.resolve(__dirname, 'node_modules/@mui/private-theming') },
        { name: '@mui/styled-engine', directory: path.resolve(__dirname, 'node_modules/@mui/styled-engine') },
      ],
    }),
  ],
};

module.exports = config;
