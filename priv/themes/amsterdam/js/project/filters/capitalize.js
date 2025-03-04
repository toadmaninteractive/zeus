angular.module('zeusApp').filter('capitalize', function(){
    return function(input, scope) {
        if (!(input && input.length > 0))
            return input;

        return input.substring(0, 1).toUpperCase() + input.substring(1);
    }
});
