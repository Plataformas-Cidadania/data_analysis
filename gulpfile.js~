var gulp = require('gulp');
var concat = require('gulp-concat');
var rename = require('gulp-rename');
var uglify = require('gulp-uglify');
var minifyJS = require('gulp-minify');
var browserSync = require('browser-sync');
var git = require('gulp-git');
var mocha = require('gulp-mocha');
var babel = require('gulp-babel');
var pump = require('pump');
var Server = require('karma').Server;
var args   = require('yargs').argv;
var editarJsFiles = 'dev/js/editar/**/*.js',
    editarJsDest = 'js/editar/',
    utilJsFiles = 'dev/js/util/**/*.js',
    utilJsDest = 'js/util/',
    helpersJsFiles = 'dev/js/helpers/**/*.js',
    helpersJsDest = 'js/helpers/',
    customJsFiles = 'dev/js/custom/**/*.js',
    customJsDest = 'js/custom/',
    cssFiles = 'css/**/*.css',
    cssDest = 'css/dist';

gulp.task('teste', function (cb) {
var t = "::::"+args.env
  pump([
    gulp.src(editarJsFiles),
     console.log(t)
  ], cb);
});

gulp.task('commit', function(){
  return gulp.src('./*')
    .pipe(git.add())
    .pipe(git.commit(args.env))
    .pipe(git.push('origin', 'master'));
});

gulp.task('pull', function(){
  git.pull('origin', ['master'], function (err) {
    if (err) throw err;
  });
});

gulp.task('push', function(){
  git.push('origin', 'master', function (err) {
    if (err) throw err;
  });
});
//minified
gulp.task('scripts', function (cb) {
  pump([
    gulp.src(editarJsFiles),
    babel({
            ignore: 'gulpfile.js',
	    presets:'es2015'
        }),
    uglify(),
    gulp.dest(editarJsDest)
  ], cb);
});

gulp.task('scripts2', function (cb) {
  pump([
    gulp.src(utilJsFiles),
    babel({
            ignore: 'gulpfile.js',
	    presets:'es2015'
        }),
    uglify(),
    gulp.dest(utilJsDest)
  ], cb);
});

gulp.task('scripts3', function (cb) {
  pump([
    gulp.src(helpersJsFiles),
    babel({
            ignore: 'gulpfile.js',
	    presets:'es2015'
        }),
    uglify(),
    gulp.dest(helpersJsDest)
  ], cb);
});

gulp.task('scripts4', function (cb) {
  pump([
    gulp.src(customJsFiles),
    babel({
            ignore: 'gulpfile.js',
	    presets:'es2015'
        }),
    uglify(),
    gulp.dest(customJsDest)
  ], cb);
});

//concatenating
gulp.task('css', function() {  
    return gulp.src(cssFiles)
        .pipe(concat('concat.css'))
        .pipe(gulp.dest(cssDest));
});
//test
gulp.task('test', function (done) {
  new Server({
    configFile: __dirname + '/karma.conf.js',
    singleRun: true
  }, done).start();
});

/**
 * Watch for file changes and re-run tests on each change
 */
gulp.task('tdd', function (done) {
  new Server({
    configFile: __dirname + '/karma.conf.js'
  }, done).start();
});

gulp.task('min', ['scripts', 'scripts2','scripts3', 'scripts4']);
