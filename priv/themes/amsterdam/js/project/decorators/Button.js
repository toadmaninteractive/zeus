angular.module('zeusApp').config(function($provide) {
    $provide.decorator('reactive', function($delegate) {
        $delegate.addRenderer('button', function(parent_container, element, context_path, context) {
            // Get bindings
            var bindings = $delegate.getBindings(element, context_path);

            // Prepare substitution values
            var substituteText = bindings.text.kind === 'binding' ? ['{{ ', bindings.text.value, ' }}'].join('') : bindings.text.value;

            // Define template
            var template = '<a href="javascript:void(null)" class="btn btn-info btn-zeus" ng-click="$event.preventDefault()">%text%</a>'
                .replace(/%text%/g, substituteText);

            // Add element
            var elemButton = angular.element(template);
            parent_container.appendChild(elemButton[0]);
        });

        return $delegate;
    });
});
