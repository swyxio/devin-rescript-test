module.exports = {
  testEnvironment: 'node',
  transform: {
    '^.+\\.bs.js$': '<rootDir>/node_modules/@glennsl/rescript-jest/src/jest.bs.js'
  },
  moduleNameMapper: {
    "rescript/lib/es6/(.*)": "<rootDir>/node_modules/rescript/lib/js/$1"
  },
  testMatch: [
    "**/src/**/?(*.)+(spec|test).bs.js",
    "**/__tests__/**/*.{js,jsx,ts,tsx}",
    "**/?(*.)+(spec|test).{js,jsx,ts,tsx}"
  ]
};
