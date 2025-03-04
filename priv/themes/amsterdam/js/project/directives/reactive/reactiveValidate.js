angular.module('zeusApp').directive('reactiveValidate', function($parse, $filter, $timeout) {
    return {
        restrict: 'A',
        link: function(scope, element, attrs) {
            var model = $parse(attrs.ngModel);

            var validateKind = element.data('validate');
            var validateMode = element.data('validate-mode');

            if (validateMode === 'text') {
                scope.$watch(function() {
                    var value = element[0].value;

                    if (!value)
                        return value;

                    switch (validateKind) {
                        case 'date':
                            if (!/^(\d{4})-(0[1-9]|1[012])-(0[1-9]|[12][0-9]|3[01])$/.test(value)) {
                                value = $filter('date')(new Date(value),'yyyy-MM-dd');
                                // HACK: preserve input text on time change
                                element[0].value = value;
                            }
                            break;
                        case 'time':
                            if (!/^(0[0-9]|1[0-9]|2[0-3]):(0[0-9]|[1-5][0-9]):(0[0-9]|[1-5][0-9])$/.test(value)) {
                                value = $filter('date')(new Date(value),'HH:mm:ss');
                                // HACK: preserve input text on date change
                                element[0].value = value;
                            }
                            break;
                    }

                    return value;
                }, function(newVal, oldVal) {
                    fnValidation(newVal, oldVal);
                }, true);
            } else {
                scope.$watch(function() {
                    return model(scope);
                }, function(newVal, oldVal) {
                    fnValidation(newVal, oldVal);
                }, true);
            }

            var fnValidation = function(newVal, oldVal) {
                if (newVal === undefined)
                    return;

                switch (validateKind) {
                    case 'number':
                        fnNumberValidate(newVal, oldVal, false);
                        break;
                    case 'float':
                        fnNumberValidate(newVal, oldVal, false);
                        break;
                    case 'integer':
                        fnNumberValidate(newVal, oldVal, true);
                        break;
                    case 'date':
                        fnSimpleRegexValidate(newVal, /^(\d{4})-(0[1-9]|1[012])-(0[1-9]|[12][0-9]|3[01])$/);
                        break;
                    case 'time':
                        fnSimpleRegexValidate(newVal, /^(0[0-9]|1[0-9]|2[0-3]):(0[0-9]|[1-5][0-9]):(0[0-9]|[1-5][0-9])$/);
                        break;
                }
            };

            var fnNumberValidate = function(newVal, oldVal, isInt) {
                var newValStr = newVal;
                var oldValStr = oldVal;

                if (!angular.isString(newVal))
                    newValStr = newVal.toString();

                if (!angular.isString(oldVal) && oldVal !== undefined && oldVal !== null)
                    oldValStr = oldVal.toString();

                var numberMatches = isInt ? newValStr.match(/^[+-]?\d+$/) : newValStr.match(/^[+-]?\d+([\.]\d+)?$/);
                var incompleteMatches = isInt ? newValStr.match(/(^[+-]$)/) : newValStr.match(/(^[+-]$)|(^[+-]?\d+[\.](0+)?$)/);
                var withoutPlusMatches = isInt ? newValStr.match(/\d+$/) : newValStr.match(/\d+([\.]\d+)?$/);

                if (newValStr === '')
                    return;

                if (numberMatches !== null) {
                    element.removeClass('invalid');

                    if (incompleteMatches !== null)
                        return;

                    var firstMatches = _.first(numberMatches);
                    var plusOnFirstPositionRegexp =  isInt ? /^[+]\d+$/ : /^[+]\d+([\.]\d+)?$/;
                    if (newValStr.match(plusOnFirstPositionRegexp) !== null)
                        firstMatches = _.first(withoutPlusMatches);

                    var valueForModel = isInt ? parseInt(firstMatches) : parseFloat(firstMatches);
                    model.assign(scope, valueForModel);
                } else {
                    if (incompleteMatches !== null)
                        element.addClass('invalid');
                    else
                        model.assign(scope, oldValStr);
                }
            };

            var fnSimpleRegexValidate = function(value, regEx) {
                if (value === '')
                    return element.removeClass('invalid');

                value.match(regEx) !== null && element.removeClass('invalid') || element.addClass('invalid');
            };
        }
    }
});
