angular.module('zeusApp').config(function($provide) {
    $provide.decorator('reactive', function($delegate, utils) {
        $delegate.addRenderer('label', function(parent_container, element, context_path, context) {
            // Update parent container style
            parent_container.style.display = 'block';
            // parent_container.style.lineHeight = '30px';

            // Get bindings
            var bindings = $delegate.getBindings(element, context_path);

            // Prepare substitution values
            var substituteColor = bindings.color.kind === 'binding' ? bindings.color.value :  ["'", (bindings.color.value || 'inherit') , "'"].join('');
            var substituteText = bindings.text.kind === 'binding' ? ('ng-bind-html="utils.toStringSafe(' + bindings.text.value + ')"') : '';
            var substituteContent = bindings.text.kind !== 'binding' ? bindings.text.value : '';

            // Render template
            var template = '<div class="zeus-label-x" ng-style="{ color: %color% }" %text%>%content%</div>'
                .replace(/%color%/g, substituteColor)
                .replace(/%text%/g, substituteText)
                .replace(/%content%/g, substituteContent);

            var elem = angular.element(template);
            parent_container.appendChild(elem[0]);
        });

        return $delegate;
    });
});
