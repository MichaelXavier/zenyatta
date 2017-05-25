import purs from "rollup-plugin-purs";

export default {
  entry: "src/Main.purs",
  dest: "dist/main.js",
  format: "iife",
  sourceMap: true,
  globals: {
    "react": "React",
    "react-dom": "ReactDOM"
  },
  plugins: [
    purs()
  ]
};
