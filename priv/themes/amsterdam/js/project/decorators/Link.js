angular.module('zeusApp').config(function($provide) {
    $provide.decorator('reactive', function($delegate, utils) {
        $delegate.addRenderer('link', function(parent_container, element, context_path, context) {
            // Get bindings
            var bindings = $delegate.getBindings(element, context_path);

            // Prepare substitution values
            var substituteText = bindings.text.kind === 'binding' ? bindings.text.value : ["\'", bindings.text.value, "\'"].join('');
            var substituteLink = 'zeus-link';
            var substituteHref = parent_container.__actions.on_click ? parent_container.__actions.on_click.href : 'javascript:';

            // Render template
            var template = '\
                <a \
                    class="link %link%" \
                    href="javascript:" \
                    ng-href="%href%" \
                    ng-bind-html="utils.toStringSafe(%text%)" \
                    ng-click="$event.preventDefault()"> \
                </a>'
                .replace(/%href%/g, substituteHref)
                .replace(/%link%/g, substituteLink)
                .replace(/%text%/g, substituteText);

            // Add element
            var elemLink = angular.element(template);
            parent_container.appendChild(elemLink[0]);
        });

        return $delegate;
    });
});
