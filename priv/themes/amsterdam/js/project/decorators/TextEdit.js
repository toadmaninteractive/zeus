angular.module('zeusApp').config(function($provide) {
    $provide.decorator('reactive', function($delegate) {
        $delegate.addRenderer('textedit', function(parent_container, element, context_path, context) {
            // Get bindings
            var bindings = $delegate.getBindings(element, context_path);

            // Define template
            var template = bindings.multiline.value === true
                ? '<textarea class="zeus-area" ng-model="%text%" placeholder="%placeholder%" />'
                : '<input type="text" ng-model="%text%" placeholder="%placeholder%" reactive-validate data-validate="%kind%" />';

            // Fill template
            var filledTemplate = template
                .replace(/%text%/g, bindings.text.value)
                .replace(/%placeholder%/g, bindings.placeholder.value || '')
                .replace(/%kind%/g, bindings.kind.value || '');

            // Add element
            var elemTextEdit = angular.element(filledTemplate);
            parent_container.appendChild(elemTextEdit[0]);
        });

        return $delegate;
    });
});
