angular.module('zeusApp').config(function($provide) {
    $provide.decorator('reactive', function($delegate) {
        $delegate.addRenderer('separator', function(parent_container, element, context_path, context) {
            // Update parent container style
            parent_container.style.display = 'block';

            // Add separator
            var elem = angular.element('<div class="horizontal-separator"></div>');
            parent_container.appendChild(elem[0]);
        });

        return $delegate;
    });
});
