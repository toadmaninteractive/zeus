angular.module('zeusApp').config(function($provide) {
    $provide.decorator('reactive', function($delegate) {
        $delegate.addRenderer('vertical', function(parent_container, element, context_path, context) {
            // Update parent container style
            parent_container.style.display = 'block';

            // Render elements vertically
            _.each(element.items, function(item) {
                var elem = angular.element('<div class="row-fluid-x"></div>');
                parent_container.appendChild(elem[0]);
                var childContainer = $delegate.render(elem[0], item, context_path);
                childContainer.__parentContainer = parent_container;
            });

            angular.element(parent_container).addClass('row-fluid-x');
        });

        return $delegate;
    });
});
