angular.module('zeusApp').config(function($provide) {
    $provide.decorator('reactive', function($delegate) {
        $delegate.addRenderer('horizontal', function(parent_container, element, context_path, context) {
            // Render elements horizontally
            _.each(element.items, function(item) {
                $delegate.render(parent_container, item, context_path, context);
            });

            angular.element(parent_container).addClass('row-fluid notdisperse horizontal');
        });

        return $delegate;
    });
});
