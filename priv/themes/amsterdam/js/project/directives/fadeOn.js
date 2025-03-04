angular.module('zeusApp').directive('fadeOn', function(){
    return {
        restrict: 'A',
        scope: {
            event: "@fadeOn"
        },
        link: function(scope, element) {
            if (!scope.event)
                return;

            scope.$on(scope.event, function() {
                element.stop(true, true).fadeToggle('fast');
            });
        }
    }
});
