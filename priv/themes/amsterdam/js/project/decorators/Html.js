angular.module('zeusApp').config(function($provide) {
    $provide.decorator('reactive', function($delegate, $parse, instance) {
        $delegate.addRenderer('html', function(parent_container, element, context_path, context) {
            // Wrap parent container
            var elemParentContainer = angular.element(parent_container);

            // Get bindings
            var bindings = $delegate.getBindings(element, context_path);

            // Define content processing function
            var fnProcessContent = function(html) {
                if (angular.isObject(html) && html.result === false)
                    html = html.error || 'Request failed';

                var result = (html || '<!-- -->')
                    .replace(/%instance_id%/g, instance.active.id)
                    .replace(/%instance_key%/g, instance.active.key);

                elemParentContainer.stop(true, true).fadeOut('fast', function() {
                    elemParentContainer.empty().stop(true, true).append(result).fadeIn('fast');
                });
            };

            if (bindings.content.kind === 'binding') {
                // Watch for data change
                $delegate.context.scope.$watch(bindings.content.value, fnProcessContent, true);
            } else {
                // Substitute content
                fnProcessContent(bindings.content.value);
            }
        });

        return $delegate;
    });
});
