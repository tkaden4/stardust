import commonjs from "@rollup/plugin-commonjs";

export default {
  input: "output/Main/index.js",
  output: {
    name: "Stardust",
    dir: "build/bundle/",
    format: "iife",
    exports: "named",
    outro: "Main.main();",
  },
  plugins: [commonjs()],
};
