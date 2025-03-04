angular.module('zeusApp').config(function($provide) {
    $provide.decorator('reactive', function($delegate) {
        $delegate.addRenderer('checkbox', function(parent_container, element, context_path, context) {
            // Wrap parent container
            var elemParentContainer = angular.element(parent_container);

            // Get bindings
            var bindings = $delegate.getBindings(element, context_path);

            // Prepare substitution values
            var substituteColor = bindings.color.kind === 'binding' ? bindings.color.value :  ["'", (bindings.color.value || 'inherit') , "'"].join('');
            var substituteText = bindings.text.kind === 'binding' ? ('ng-bind-html="utils.toStringSafe(' + bindings.text.value + ')"') : '';
            var substituteContent = bindings.text.kind !== 'binding' ? bindings.text.value : '';

            // Define template
            var template = '\
                <div class="controls"> \
                    <label class="checkbox"> \
                        <input type="checkbox" ng-model="%checked%" uniform /> \
                        <div ng-style="{ color: %color% }" %text%>%content%</div> \
                    </label> \
                </div>'
                .replace(/%checked%/g, bindings.checked.value)
                .replace(/%color%/g, substituteColor)
                .replace(/%text%/g, substituteText)
                .replace(/%content%/g, substituteContent);

            // Add element
            var elemCheckBox = angular.element(template);
            parent_container.appendChild(elemCheckBox[0]);

            // Add on_change event handler
            angular.element('input[type="checkbox"]', elemCheckBox).on('click', function() {
                elemParentContainer.trigger('zeus:on:change');
            });
        });

        return $delegate;
    });
});
