angular.module('zeusApp').config(function($provide) {
    $provide.decorator('reactive', function($delegate) {
        $delegate.addRenderer('inplace_textedit', function(parent_container, element, context_path, context) {
            // Get bindings
            var bindings = $delegate.getBindings(element, context_path);

            // Add notification function
            var auxId = $delegate.generateAuxId();
            var notificationFunPath = [$delegate.contextPath, 'data.aux', auxId].join('.');

            $delegate.context.data.aux[auxId] = function() {
                angular.element(parent_container).trigger('zeus:on:change');
            };

            // Define template
            var template = '\
                <div class="inplace-container"> \
                    <a href="javascript:" \
                        class="zeus-inplace" \
                        editable-text="%text%" \
                        onaftersave="%notify%()"> \
                            {{ %text% ? %text% : \'&lt;No value&gt;\' }} \
                    </a> \
                </div>'
                .replace(/%text%/g, bindings.text.value)
                .replace(/%notify%/g, notificationFunPath);

            // Add element
            var elemInplaceTextEdit = angular.element(template);
            parent_container.appendChild(elemInplaceTextEdit[0]);

        });

        return $delegate;
    });
});
