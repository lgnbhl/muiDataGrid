const webpack = require('webpack');
const path = require('path');

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
  plugins: [new webpack.DefinePlugin({ 'process.env': '{}' })],
};

module.exports = config;
