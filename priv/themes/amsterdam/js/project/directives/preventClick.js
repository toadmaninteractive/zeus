angular.module('zeusApp').directive('preventClick', function(){
    return {
        restrict: 'A',
        link: function(scope, element) {
            angular.element(element).click(function(event) {
                event.preventDefault();
            });
        }
    }
});
