angular.module('zeusApp').config(function($provide) {
    $provide.decorator('reactive', function($delegate, $parse, $timeout, utils) {
        $delegate.addRenderer('property_grid', function(parent_container, element, context_path, context) {
            // Update parent container style
            parent_container.style.display = 'block';

            // Get bindings and layouts
            var bindings = $delegate.getBindings(element, context_path);

            // Define tree data
            var treeDataId = $delegate.generateAuxId();
            var gridContextPath = [$delegate.contextPath, 'data.aux', treeDataId].join('.');
            $delegate.context.data.aux[treeDataId] = [
                { Name: '<Root>', value: '[Empty Array]' }
            ];

            var colDataId = $delegate.generateAuxId();
            var colContextPath = [$delegate.contextPath, 'data.aux', colDataId].join('.');
            $delegate.context.data.aux[colDataId] = [
                { displayName: 'Value', field: 'value', sortable: false, filterable: false }
            ];

            var treeControlId = $delegate.generateAuxId();
            var treeControlContextPath = [$delegate.contextPath, 'data.aux', treeControlId].join('.');
            $delegate.context.data.aux[treeControlId] = {};

            // Render content
            var content;
            if (bindings.value.kind === 'binding') {
                content = '\
                    <tree-grid \
                        class="bordered padded" \
                        tree-data="%value%" \
                        tree-control="%control%" \
                        col-defs="%columns%" \
                        expand-level="%expand%"></tree-grid>'
                    .replace(/%value%/g, gridContextPath)
                    .replace(/%control%/g, treeControlContextPath)
                    .replace(/%columns%/g, colContextPath)
                    .replace(/%expand%/g, (bindings.collapsed.value ? 0: 100).toString());
            } else {
                content = bindings.value.kind === 'binding'
                    ? ['<span ng-bind-html="', bindings.value.value, '"></span>'].join('')
                    : bindings.value.value;
            }

            var elemBlock = angular.element(content);
            parent_container.appendChild(elemBlock[0]);

            var fnConvertData = function(data, name) {
                var result;
                var displayName = name || '<Root>';

                if (data === undefined) {
                    result = { Name: displayName, value: 'undefined' };
                } else if (data === null) {
                    result = { Name: displayName, value: 'null' };
                } else if (angular.isString(data)) {
                    result = { Name: displayName, value: data };
                } else if (angular.isNumber(data)) {
                    result = { Name: displayName, value: data };
                } else if (typeof data === 'boolean') {
                    result = { Name: displayName, value: data };
                } else if (angular.isArray(data)) {
                    result = { Name: displayName, value: data.length === 0 ? '' : '' };

                    if (data.length > 0) {
                        result.children = _.map(data, function(item, index) {
                            return fnConvertData(item, '[' + index + ']');
                        });
                    }
                } else if (angular.isObject(data)) {
                    result = { Name: displayName, value: angular.equals(data, {}) ? '' : '' };

                    if (!angular.equals(data, {})) {
                        result.children = _.map(_.keys(data), function(key) {
                            return fnConvertData(data[key], key);
                        });
                    }
                }

                return name ? result : [result];
            };

            if (bindings.value.kind === 'binding') {
                var modelValue = $parse(bindings.value.value);
                $delegate.context.data.aux[treeDataId] = fnConvertData(modelValue($delegate.context.scope));

                $delegate.context.scope.$watch(function() {
                    return modelValue($delegate.context.scope);
                }, function(new_val, old_val) {
                    if (!angular.equals(new_val, old_val)) {
                        $delegate.context.data.aux[treeDataId] = fnConvertData(new_val);

                        if (!bindings.collapsed.value) {
                            $timeout(function () {
                                $delegate.context.data.aux[treeControlId].expand_all();
                            }, 100);
                        }
                    }
                }, true);
            }
        });

        return $delegate;
    });
});
