angular.module('zeusApp').config(function($provide) {
    $provide.decorator('reactive', function($delegate, $parse, $interpolate, $timeout) {
        $delegate.addRenderer('combobox', function(parent_container, element, context_path, context) {
            // Get bindings
            var bindings = $delegate.getBindings(element, context_path);

            // Define items path
            var itemsPath = bindings.items.value;

            // Inject implicit items
            if (bindings.items.kind !== 'binding') {
                var auxId = $delegate.generateAuxId();
                $delegate.context.data.aux[auxId] = bindings.items.value;
                itemsPath = [$delegate.contextPath, '.data.aux.', auxId].join('');
            }

            // Define models
            var modelItems = $parse(itemsPath);
            var modelSelected = $parse(bindings.selected.value);

            // Add select
            var elemSelect = angular.element('<select class="clickable" select2 data-width1="300" data-allow-clear="yes" data-placeholder="Select..."></select>');
            elemSelect[0].style.paddingRight = '6px';
            parent_container.appendChild(elemSelect[0]);

            // Define option template
            var templateOption = '<option value="%value%">%caption%</option>';

            // Define redraw function
            var fnRedraw = function(items) {
                if (!angular.isArray(items))
                    return;

                // Clear container
                elemSelect.empty();

                // Add empty option
                var elemOption = angular.element('<option></option>');
                elemSelect[0].appendChild(elemOption[0]);

                for (var index = 0; index < items.length; index++) {
                    // Define option context
                    var contextOption = [itemsPath, '[', index.toString(), ']'].join('');

                    // Get caption binding
                    var bindingCaption = $delegate.getBinding(element.caption, contextOption);

                    // Prepare substitution values
                    var substituteCaption = bindingCaption.kind === 'binding'
                        ? $parse(bindingCaption.value)($delegate.context.scope)
                        : bindingCaption.kind === 'content'
                            ? $interpolate(bindingCaption.value)($delegate.context.scope)
                            : items[index];

                    // Create option
                    var option = templateOption
                        .replace(/%value%/g, index.toString())
                        .replace(/%caption%/g, substituteCaption);

                    // Add option to select
                    elemOption = angular.element(option);
                    elemSelect[0].appendChild(elemOption[0]);
                }

                // Set selected value
                var selectedValue = modelSelected($delegate.context.scope);
                var selectedIndex = items.indexOf(selectedValue);

                if (selectedIndex !== -1)
                    elemSelect.val(selectedIndex.toString());
            };

            if (bindings.items.kind === 'binding') {
                // Watch for items change
                $delegate.context.scope.$watch(itemsPath, function (new_value, old_value) {
                    fnRedraw(new_value);
                }, true);
            } else {
                // Redraw immediately
                fnRedraw(bindings.items.value);
            }

            // Watch for selected value change
            $delegate.context.scope.$watch(bindings.selected.value, function(new_value, old_value) {
                // Get items
                var items = modelItems($delegate.context.scope);

                if (!angular.isArray(items))
                    return;

                // Set selected value
                var selectedIndex = items.indexOf(new_value);

                if (selectedIndex !== -1) {
                    elemSelect.val(selectedIndex.toString());
                    elemSelect.trigger('change');
                }
            }, true);

            // Handle change event
            elemSelect.on('change', function() {
                // Evaluate models
                var items = modelItems($delegate.context.scope);
                var selectedValue = modelSelected($delegate.context.scope);

                // Check if items is array
                if (!angular.isArray(items))
                    return;

                // Get selected index and inner value
                var selectedIndex = items.indexOf(selectedValue);
                var selectedInnerValue = parseInt(elemSelect.val());

                if (selectedIndex !== selectedInnerValue) {
                    modelSelected.assign($delegate.context.scope, items[selectedInnerValue]);

                    $timeout(function() {
                        angular.element(parent_container).trigger('zeus:on:change');
                    }, 50);
                }
            })
        });

        return $delegate;
    });
});
