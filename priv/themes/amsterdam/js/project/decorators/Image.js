angular.module('zeusApp').config(function($provide) {
    $provide.decorator('reactive', function($delegate, instance) {
        $delegate.addRenderer('image', function(parent_container, element, context_path, context) {
            // Get bindings
            var bindings = $delegate.getBindings(element, context_path);

            // Prepare substitution values
            var substituteScale = bindings.scale.value === true ? 'fit-width' : '';
            var substituteSource = bindings.source.kind === 'binding'
                ? "{{ %source%.indexOf('://') === -1 && ('_utils/api/instances/link/%instance_id%/' + %source%) || %source% }}"
                    .replace(/%source%/g, bindings.source.value)
                    .replace(/%instance_id%/g, instance.active.id)
                : bindings.source.value.indexOf('://') === -1
                    ? ['_utils/api/instances/link/', instance.active.id, '/', bindings.source.value].join('')
                    : bindings.source.value;

            // Render template
            var template = '<img class="zeus-image %scale%" ng-src="%source%" />'
                .replace(/%scale%/g, substituteScale)
                .replace(/%source%/g, substituteSource);

            var elem = angular.element(template);
            parent_container.appendChild(elem[0]);
        });

        return $delegate;
    });
});
