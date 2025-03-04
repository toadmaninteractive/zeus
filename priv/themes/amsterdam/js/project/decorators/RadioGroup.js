angular.module('zeusApp').config(function($provide) {
    $provide.decorator('reactive', function($delegate, $parse, $interpolate, $timeout, utils) {
        $delegate.addRenderer('radiogroup', function(parent_container, element, context_path, context) {
            // Update parent container style
            parent_container.style.display = 'block';

            // Get bindings
            var bindings = $delegate.getBindings(element, context_path);

            // Prepare substitution values
            var substituteModel;

            if (bindings.value.kind === 'binding') {
                substituteModel = bindings.value.value;
            } else {
                var auxId = $delegate.generateAuxId();
                $delegate.context.data.aux[auxId] = null;
                substituteModel = [$delegate.contextPath, '.data.aux.', auxId].join('');
            }

            var substituteName = 'group_' + $delegate.generateAuxId();
            var substituteSeparator = bindings.vertical_order.value === true ? '<div class="radio-separator"></div>' : '';
            var substituteValue = $parse(substituteModel)($delegate.context.scope);

            // Define radio item and content templates
            var templateRadioItem = '\
                <label class="radio inline clickable"> \
                    <input class="style" type="radio" name="%name%" value="%value%" ng-model="%model%" uniform /> \
                    <span>%caption%</span> \
                </label>'
                .replace(/%name%/g, substituteName)
                .replace(/%model%/g, substituteModel);

            var templateRadioContent = '<div class="radio-item-content"></div>';

            // Define hash map for values
            var hashMap = {};

            // Add radio group items
            _.each(element.items, function(item) {
                // Get item bindings
                var bindingCaption = $delegate.getBinding(item.caption, context_path);
                var bindingValue = $delegate.getBinding(item.value, context_path);

                // Prepare item substitution values
                var substituteItemCaption = bindingCaption.kind === 'binding'
                    ? $parse(bindingCaption.value)($delegate.context.scope)
                    : angular.isString(bindingCaption.value)
                        ? $interpolate(bindingCaption.value)($delegate.context.scope)
                        : (bindingCaption.value + '');

                var substituteItemValue = bindingValue.kind === 'binding'
                    ? $parse(bindingValue.value)($delegate.context.scope)
                    : angular.isString(bindingValue.value)
                        ? $interpolate(bindingValue.value)($delegate.context.scope)
                        : ('rnd_' + utils.randomString());

                // Fill radio item template
                var templateItem = templateRadioItem
                    .replace(/%value%/g, substituteItemValue)
                    .replace(/%caption%/g, substituteItemCaption);

                // Add radio item
                var elemRadioItem = angular.element(templateItem);
                parent_container.appendChild(elemRadioItem[0]);

                // Add separator
                if (substituteSeparator)
                    parent_container.appendChild(angular.element(substituteSeparator)[0]);

                // Add on-change handler
                angular.element('input[type="radio"]', elemRadioItem).on('change', function() {
                    var value = angular.element(this).val();
                    var opts = _.map(_.values(hashMap), function(opt) {
                        return opt[0];
                    });

                    angular.element(opts).hide();
                    hashMap[value].show();
                    angular.element(parent_container).trigger('zeus:on:change');
                });

                var elemContent;

                // Add content if present
                if (item.template) {
                    // Add item content
                    elemContent = angular.element(templateRadioContent);
                    hashMap[substituteItemValue] = elemContent;

                    // Render layout or add textual content
                    if (angular.isObject(item.template))
                        $delegate.render(elemContent[0], item.template, context_path, context);
                    else
                        elemContent.html(item.template + '');

                    // Add separator
                    if (substituteSeparator)
                        elemContent[0].appendChild(angular.element(substituteSeparator)[0]);

                    // Update item content visibility
                    if (substituteItemValue !== substituteValue)
                        elemContent[0].style.display = 'none';

                    // Add to parent container for vertical alignment
                    if (bindings.vertical_order.value)
                        parent_container.appendChild(elemContent[0]);
                }
            });

            // Add item contents after radio buttons for horizontal alignment
            if (!bindings.vertical_order.value) {
                for (var opt in hashMap) {
                    parent_container.appendChild(hashMap[opt][0]);
                }
            }
        });

        return $delegate;
    });
});
