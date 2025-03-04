angular.module('zeusApp').directive('preventBubble', function(){
    return {
        restrict: 'A',
        link: function(scope, element) {
            angular.element(element).click(function(event) {
                event.stopPropagation();
            });
        }
    }
});
