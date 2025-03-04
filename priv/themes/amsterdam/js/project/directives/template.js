angular.module('zeusApp').directive('template', function($compile, storage){
    return {
        restrict: 'A',
        // transclude: true,
        link: function(scope, element, attrs) {
            var useScope;
            var useTemplate = attrs.template;
            var defaultContent = '<!-- -->';

            var fnReplaceAndCompile = function(data) {
                // Destroy and re-create template scope
                angular.isDefined(useScope) && useScope.$destroy();
                useScope = scope.$new(false, scope);

                // Replace and compile element contents
                var html = angular.isObject(data) ? data.data : data;
                element.html(html || defaultContent);
                $compile(element.contents())(useScope);
            };

            var fnApply = function(template) {
                if (angular.isString(template) && template.length > 0) {
                    storage.getTemplate(template, fnReplaceAndCompile);
                } else {
                    fnReplaceAndCompile(defaultContent)
                }
            };

            // Apply template
            fnApply(useTemplate);

            // Observe for template changes
            attrs.$observe('template', function(value) {
                if (value !== useTemplate) {
                    useTemplate = value;
                    fnApply(useTemplate);
                }
            });
        }
    }
});
