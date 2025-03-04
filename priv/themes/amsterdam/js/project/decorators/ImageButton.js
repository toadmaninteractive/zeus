angular.module('zeusApp').config(function($provide) {
    $provide.decorator('reactive', function($delegate) {
        $delegate.addRenderer('imagebutton', function(parent_container, element, context_path, context) {
            // Get bindings
            var bindings = $delegate.getBindings(element, context_path);

            // Prepare substitution values
            var substituteIcon = bindings.icon.kind === 'binding' ? ['{{ ', bindings.icon.value, ' }}'].join('') : bindings.icon.value;
            var substituteTooltip = bindings.tooltip.kind === 'binding' ? ['{{ ', bindings.tooltip.value, ' }}'].join('') : (bindings.tooltip.value || '');

            // Define template
            var template = '\
                <div class="btn hovertip mini clickable" tooltip="%tooltip%"> \
                    <i class="%icon%" style="margin: 2px 0 0 2px"></i> \
                </div>'
                .replace(/%icon%/g, substituteIcon)
                .replace(/%tooltip%/g, substituteTooltip);

            // Add element
            var elemImageButton = angular.element(template);
            parent_container.appendChild(elemImageButton[0]);
        });

        return $delegate;
    });
});
