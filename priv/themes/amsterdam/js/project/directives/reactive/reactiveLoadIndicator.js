angular.module('zeusApp').directive('reactiveLoadIndicator', function() {
    return {
        restrict: 'A',
        scope: {
            hookEvent: '@'
        },
        link: function(scope, element, attrs) {
            // Hide loader
            element.hide();

            // Skip processing if no valid data supplied
            if (!scope.hookEvent)
                return;

            scope.$on(scope.hookEvent, function($event, data) {
                !!data && element.stop(true, true).fadeIn('fast') || element.stop(true, true).fadeOut('fast');
            });
        }
    }
});
