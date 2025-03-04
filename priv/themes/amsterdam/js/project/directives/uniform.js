angular.module('zeusApp').directive('uniform', function($parse) {
    return {
        link: function(scope, element, attrs) {
            if (element.hasClass('x-uniformed'))
                return;

            element.addClass('x-uniformed').uniform({ radioClass: 'choice', selectAutoWidth: false });

            attrs.$observe('isChecked', function(value) {
                if (attrs.isChecked === 'true' || attrs.isChecked === 'false') {
                    element.attr('checked', attrs.isChecked === 'true');
                    $.uniform.update(element);
                }
            });

            attrs.$observe('isDisabled', function(value) {
                if (attrs.isDisabled === 'true' || attrs.isDisabled === 'false') {
                    element.attr('disabled', attrs.isDisabled === 'true');
                    $.uniform.update(element);
                }
            });

            if (attrs.ngModel) {
                var model = $parse(attrs.ngModel);

                scope.$watch(function() {
                    return model(scope);
                }, function() {
                    $.uniform.update(element);
                }, true);
            }

            scope.$on('$destroy', function() {
                $.uniform.restore(element);
            });
        }
    }
});
