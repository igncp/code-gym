import typescript from 'rollup-plugin-typescript';
import uglify from 'rollup-plugin-uglify';

export default {
  entry: './src/main.ts',
  dest: './build/build.js',

  plugins: [
    typescript(),
    uglify()
  ]
}
