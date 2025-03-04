angular.module('zeusApp').config(function($provide) {
    $provide.decorator('reactive', function($delegate, $compile, $timeout, utils) {
        $delegate.addRenderer('listing', function(parent_container, element, context_path, context) {
            // Get bindings and layouts
            var bindings = $delegate.getBindings(element, context_path);
            var layouts = $delegate.getLayouts(element, context_path);

            // Add list
            var elemList = angular.element('<ul></ul>');
            parent_container.appendChild(elemList[0]);
            parent_container.style.display = 'block';

            // Define list item template
            var templateListItem = '<li class="%separated%" style="display: block;"></li>'
                .replace(/%separated%/g, bindings.show_separator.value ? 'separated' : '');

            // Define initial render flag
            var initiallyRendered = false;

            // Watch for items collection change
            $delegate.context.scope.$watch(bindings.items.value, function(new_value, old_value) {
                // FIXME: triggers re-render on array item data change (e.g., when bound to inputs)
                var isShallowChange = false;

                if (angular.isArray(old_value) && angular.isArray(new_value)) {
                    // NB: array ref is not kept
                    var isSameLength = old_value.length === new_value.length;

                    if (isSameLength) {
                        var isShallowEqual = old_value
                            .map((e, index) => e === new_value[index])
                            .filter(equals => equals)
                            .length === 0;

                        if (isShallowEqual) {
                            isShallowChange = true;
                        }
                    }
                }

                var shouldRender = initiallyRendered
                    ? angular.isArray(new_value)
                        ? !isShallowChange
                        : !angular.equals(new_value, old_value)
                    : true;

                if (!(shouldRender && angular.isArray(new_value)))
                    return;

                // Set initial render flag if not set before
                if (!initiallyRendered)
                    initiallyRendered = true;

                // Empty list
                elemList.empty();

                // Loop over items collection
                for (var index = 0; index < new_value.length; index++) {
                    // Define item context
                    var itemContextPath = [bindings.items.value, '[', index.toString(), ']'].join('');
                    var itemContext = new_value[index];

                    // Add list item to list
                    var elemListItem = angular.element(templateListItem);
                    elemList[0].appendChild(elemListItem[0]);

                    // Render item
                    $delegate.render(elemListItem[0], layouts.item_template, itemContextPath, itemContext);
                }

                // Recompile list
                $compile(elemList.contents())($delegate.context.scope);
            }, true);

            // Add listener to page:load event
            var handled = false;

            $delegate.context.scope.$on('page:load', function($event, opts) {
                if (handled)
                    return;

                handled = true;

                // FIXME: resulted in multiple onLoad calls, consider removal
                /*
                $timeout(function() {
                    utils.broadcast('page:load', { counter: opts.counter });
                }, 100);
                */
            });
        });

        return $delegate;
    });
});
