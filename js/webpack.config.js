const webpack = require('webpack');
const path = require('path');
const { LicenseWebpackPlugin } = require('license-webpack-plugin');

// This project builds with yarn (yarn.lock is the only lockfile; see
// ../reinstall.sh and .github/workflows/js-bundle-check.yaml). The
// "resolutions" pins in package.json are load-bearing:
//
// "react-is": "18.3.1" — shiny.react provides a React 18 runtime, but
// @mui/x-data-grid declares react-is ^19. react-is 19 detects elements by
// React 19's new Symbol.for('react.transitional.element') tag and does NOT
// recognize React 18 elements (Symbol.for('react.element')), so bundling it
// would silently break isElement/isFragment checks at runtime. Keep the pin
// at the react-is major matching the React runtime shiny.react ships. To
// verify a build: the bundle must contain no 'react.transitional.element'.
//
// "@mui/x-internals": "9.1.0" — @mui/x-data-grid 9.5.0 ships
// @mui/x-virtualizer 9.0.0-alpha.8, which imports isFirefox/isJSDOM from
// @mui/x-internals/platform; x-internals 9.8.0 (still within the declared
// ^9.1.0 range) removed those exports and breaks the build. Drop this pin
// when bumping @mui/x-data-grid to a version whose dependency pair is
// coherent again (the build simply fails if it is not).

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
        // MUI now ships .mjs (strict ESM) files that use extension-less imports
        // (e.g. react-transition-group/TransitionGroupContext). webpack 5 treats
        // .mjs as fully specified and refuses to resolve them; relax that here.
        test: /\.m?js$/,
        resolve: { fullySpecified: false },
      },
      {
        test: /\.css$/,
        use: ['style-loader', 'css-loader'],
      },
    ],
  },
  // The Data Grid is bundled as a single Shiny dependency and is inherently
  // large (~2 MiB); it cannot be code-split here, so disable the size budget
  // warnings rather than chase an unachievable limit.
  performance: { hints: false },
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
