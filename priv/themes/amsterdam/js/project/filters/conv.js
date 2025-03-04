angular.module('zeusApp').filter('conv', function($window) {
    return function(input, converter) {
        return angular.isObject($window.ZeusConverters) && angular.isFunction($window.ZeusConverters[converter])
            ? $window.ZeusConverters[converter](input)
            : input;
    }
});
