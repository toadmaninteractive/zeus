angular.module('zeusApp').directive('focusOn', function(){
    return {
        restrict: 'A',
        link: function(scope, element, attrs) {
            if (angular.isString(attrs.focusOn)) {
                scope.$on(attrs.focusOn, function () {
                    element.focus();
                });
            }
        }
    }
});
