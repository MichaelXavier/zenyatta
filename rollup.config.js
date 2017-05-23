import purs from "rollup-plugin-purs";

export default {
  entry: "src/Main.purs",
  dest: "dist/main.js",
  format: "iife",
  sourceMap: true,
  plugins: [
    purs()
  ]
};
