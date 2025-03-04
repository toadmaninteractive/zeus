angular.module('zeusApp').directive('color', function() {
    return {
        restrict: 'E',
        link: function(scope, element, attrs) {
            attrs.x && element.css({ color: attrs.x });
        }
    }
});
