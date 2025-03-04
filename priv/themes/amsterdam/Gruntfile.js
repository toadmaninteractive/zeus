module.exports = function(grunt) {
    grunt.initConfig({
        // pkg: grunt.file.readJSON('package.json'),

        clean: ['.tmp'],

        useminPrepare: {
            html: 'index.html',
            options: {
                root: './',
                dest: './'
            },
            flow: {
                steps: {
                    js: ['concat', 'uglifyjs'],
                    css: ['concat', 'cssmin'] },
                post: {}
            }
        },

        uglify: {
            options: {
                report: 'min',
                mangle: false
            }
        },

        'string-replace': {
            inline: {
                files: {
                    'dist/': ['dist/*.css']
                },
                options: {
                    replacements: [
                        {
                            pattern: /\.tmp\/concat/ig,
                            replacement: '..'
                        }
                    ]
                }
            }
        }
    });

    grunt.loadNpmTasks('grunt-contrib-clean');
    grunt.loadNpmTasks('grunt-contrib-copy');
    grunt.loadNpmTasks('grunt-contrib-concat');
    grunt.loadNpmTasks('grunt-contrib-cssmin');
    grunt.loadNpmTasks('grunt-contrib-uglify');
    grunt.loadNpmTasks('grunt-usemin');
    grunt.loadNpmTasks('grunt-string-replace');

    // Tell Grunt what to do when we type 'grunt' into the terminal
    grunt.registerTask('default', [
        'clean', 'useminPrepare', 'concat', 'uglify', 'cssmin', 'string-replace', 'clean'
    ]);
};
