angular.module('zeusApp').directive('select2', function($timeout, $interpolate) {
    return {
        scope: true,
        restrict: 'AC',
        link: function(scope, element, attrs) {
            if (element.hasClass('x-select2'))
                return;

            element.addClass('x-select2');

            var params = { minimumResultsForSearch: 5, allowClear: false };

            switch (element.data('preset')) {
                case 'default': params.width = 200; break;
                case 'liquid': params.width = 'off'; break;
                case 'full': params.width = '100%'; break;
                case 'half': params.width = '50%'; break;
                case '1/3': params.width = '33%'; break;
                case '2/3': params.width = '66%'; break;
            }

            var selected = scope.$eval(element.data('selected'));
            var disabled = element.data('disable');

            if (angular.isString(disabled) && disabled.indexOf('{{') !== -1)
                disabled = $interpolate(disabled)(scope) === 'true';

            // Let angular initialize items
            $timeout(function() {
                var width = element.data('width');
                width && (params.width = new RegExp('^[0-9]+$', 'g').test(width) ? parseInt(width) : width);
                element.data('min-results') && (params.minimumResultsForSearch = element.data('min-results'));
                element.data('max-selection') && (params.maximumSelectionSize = element.data('max-selection'));
                element.data('allow-clear') && (params.allowClear = element.data('allow-clear') === 'yes');
                element.data('placeholder') && (params.placeholder = element.data('placeholder'));

                selected && angular.element('option[value="%selected%"]'.replace(/%selected%/g, selected)).attr('selected', true);
                element.select2(params);
                disabled && element.select2('enable', false);

                attrs.$observe('disable', function(val) {
                    val !== disabled && (disabled = val);
                    typeof disabled === 'string' && (disabled = disabled === 'true');
                    element.select2('enable', !disabled);
                });

                scope.$on('$destroy', function() {
                    element.select2('destroy');
                });

                if (attrs.ngModel) {
                    scope.$watch(attrs.ngModel, function(new_val, old_val) {
                        $timeout(function() {
                            if (new_val !== old_val)
                                element.trigger('change');
                        }, 0);
                    });
                }
            }, 0);
        }
    }
});
