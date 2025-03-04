angular.module('zeusApp').service('reactive', function($document, $compile, $parse, $interpolate, $filter, $timeout, instance, recent, api, utils, RenderSchema) {
    // Linked context
    var context;
    var contextPath;

    // Internal data
    var counters = { nodes: 0, aux: 0, tabs: 0, dropdown: 0, onLoad: 0 };
    var elementMap = {};

    // Schema element renderers
    var renderers = {
        unknown: function(parent_container, element, context_path, context) {
            var content = '<span class="label label-important">%content%</span>'
                .replace(/%content%/g, 'Unknown element type: ' + element.type);

            var div = angular.element(content);
            parent_container.appendChild(div[0]);
        }
    };

    // Linked context handling
    Object.defineProperty(this, 'context', {
        enumerable: true,
        get: function() {
            return context;
        }
    });

    Object.defineProperty(this, 'contextPath', {
        enumerable: true,
        get: function() {
            return contextPath;
        }
    });

    Object.defineProperty(this, 'counters', {
        enumerable: true,
        get: function() {
            return counters;
        }
    });

    this.assignContext = function(context_obj, context_path) {
        context = context_obj;
        contextPath = context_path;
    };

    // Internal routing
    this.isInternalRoute = function(route) {
        return ['', '/_admin', '/_logs'].indexOf(route) !== -1;
    };

    // Element container lookup
    this.containerById = function(id) {
        return elementMap[id];
    };

    // ID generation
    this.generateNodeId = function() {
        return 'z.' + (counters.nodes++).toString(36);
    };

    this.generateAuxId = function() {
        return 'aux_' + (counters.aux++).toString(36);
    };

    this.generateTabId = function() {
        return 'tab_' + (counters.tabs++).toString(36);
    };

    this.generateDropdownId = function() {
        return 'dropdown_' + (counters.dropdown++).toString(36);
    };

    // Binding handling
    this.isBinding = function(obj) {
        return angular.isObject(obj) && obj.hasOwnProperty('binding');
    };

    this.isContent = function(obj) {
        return angular.isObject(obj) ? (!obj.binding && !!obj.content) : false;
    };

    this.isInterpolatable = function(value) {
        return angular.isString(value) && value.indexOf('{{') !== -1;
    };

    this.hasConverter = function(binding) {
        return angular.isObject(binding) && angular.isString(binding.converter) && binding.converter.length > 0;
    };

    this.bindingToPath = function(binding, context_path) {
        var rootContext = contextPath + '.data.page';

        if (!this.isBinding(binding))
            return context_path || rootContext;

        if (binding.binding.length === 0)
            return context_path || rootContext;

        var bindingParts = angular.copy(binding.binding).splice(1).join('.');

        // Bind to root data context
        if (binding.binding[0] === 'R')
            return binding.binding.length > 1 ? (rootContext + '.' + bindingParts) : rootContext;

        // Bind to parent data context
        if (binding.binding[0] === 'P') {
            if (!context_path)
                return binding.binding.length > 1 ? (rootContext + '.' + bindingParts) : rootContext;

            var parts = _.without(context_path.split(/(\[[0-9]+\]|\.)/g), '', '.', undefined);
            parts.pop();
            var parentContext = parts.join('.').replace(/\.\[/g, '[');

            return binding.binding.length > 1 ? (parentContext + '.' + bindingParts) : parentContext;
        }

        // Bind to qs data context
        if (binding.binding[0] === 'Q')
            return binding.binding.length > 1 ? (contextPath + '.data.qs.' + bindingParts) : (contextPath + '.data.qs');

        // Bind to current data context
        return context_path
            ? (context_path + '.' + binding.binding.join('.'))
            : (rootContext + '.' + binding.binding.join('.'));
    };

    this.contentInterpolate = function(str, context_path) {
        if (!angular.isString(str))
            return str;

        var rootContext = contextPath + '.data.page';
        var currentContext = context_path || rootContext;
        var parts = _.without(currentContext.split(/(\[[0-9]+\]|\.)/g), '', '.', undefined);
        parts.pop();
        var parentContext = parts.join('.').replace(/\.\[/g, '[');

        return str
            .replace(/%%/g, currentContext)
            .replace(/%P%/g, parentContext.indexOf(rootContext) === -1 ? rootContext : parentContext)
            .replace(/%R%/g, rootContext)
            .replace(/%Q%/g, contextPath + '.data.qs');
    };

    this.bindingValue = function(binding, context_path) {
        var value = this.isBinding(binding)
            ? this.bindingToPath(binding, context_path)
            : this.isContent(binding)
                ? this.contentInterpolate(binding.content, context_path)
                : angular.isString(binding)
                    ? this.contentInterpolate(binding, context_path)
                    : binding;

        var valueSubstitute = this.isBinding(binding)
            ? value
            : angular.isString(value)
                ? ('\'' + value + '\'')
                : (value === null || angular.isUndefined(value))
                    ? value
                    : value.toString();

        return this.hasConverter(binding)
            ? '%context%.convert(\'%converter%\', %value%)'
                .replace(/%context%/g, contextPath)
                .replace(/%converter%/g, binding.converter)
                .replace(/%value%/g, valueSubstitute)
            : value;
    };

    // Utility functions
    this.getBinding = function(binding, context_path) {
        var that = this;

        var fnProcessBinding = function(item) {
            var kind = 'value';

            if (that.isBinding(item))
                kind = 'binding';
            else if (that.isContent(item))
                kind = 'content';

            var interpolatable = kind !== 'binding'
                ? that.isInterpolatable(kind === 'content' ? item.content : item)
                : false;

            var value = that.bindingValue(item, context_path);

            return {
                binding: item,
                kind: kind,
                interpolatable: interpolatable,
                value: value
            };
        };

        // Regular binding
        return fnProcessBinding(binding);
    };

    this.getBindings = function(element, context_path) {
        if (!(angular.isObject(element) && angular.isString(element.type)))
            return {};

        var elementDescriptor = angular.isObject(RenderSchema.elements[element.type]) ? RenderSchema.elements[element.type] : {};
        var commonKeys = angular.isArray(RenderSchema.common.bindings) ? RenderSchema.common.bindings : [];
        var elementKeys = angular.isArray(elementDescriptor.bindings) ? elementDescriptor.bindings : [];
        var keys = _.uniq(_.union(commonKeys, elementKeys));
        var result = {};
        var that = this;

        var fnProcessBinding = function(item) {
            var kind = 'value';

            if (that.isBinding(item))
                kind = 'binding';
            else if (that.isContent(item))
                kind = 'content';

            var interpolatable = kind !== 'binding'
                ? that.isInterpolatable(kind === 'content' ? item.content : item)
                : false;

            var value = that.bindingValue(item, context_path);

            return {
                binding: item,
                kind: kind,
                interpolatable: interpolatable,
                value: value
            };
        };

        _.each(keys, function(key) {
            if (angular.isArray(element[key])) {
                // Bindings array
                result[key] = [];

                for (var index = 0; index < element[key].length; index++) {
                    result[key].push(fnProcessBinding(element[key][index]));
                }
            } else {
                // Regular binding
                result[key] = fnProcessBinding(element[key]);
            }
        });

        return result;
    };

    this.getEvents = function(element) {
        if (!(angular.isObject(element) && angular.isString(element.type)))
            return {};

        var elementDescriptor = angular.isObject(RenderSchema.elements[element.type]) ? RenderSchema.elements[element.type] : {};
        var commonKeys = angular.isArray(RenderSchema.common.events) ? RenderSchema.common.events : [];
        var elementKeys = angular.isArray(elementDescriptor.events) ? elementDescriptor.events : [];
        var keys = _.uniq(_.union(commonKeys, elementKeys));
        var result = {};

        _.each(keys, function(key) {
            result[key] = element[key];
        });

        return result;
    };

    this.getLayouts = function(element) {
        if (!(angular.isObject(element) && angular.isString(element.type)))
            return {};

        var elementDescriptor = angular.isObject(RenderSchema.elements[element.type]) ? RenderSchema.elements[element.type] : {};
        var commonKeys = angular.isArray(RenderSchema.common.layouts) ? RenderSchema.common.layouts : [];
        var elementKeys = angular.isArray(elementDescriptor.layouts) ? elementDescriptor.layouts : [];
        var keys = _.uniq(_.union(commonKeys, elementKeys));
        var result = {};

        _.each(keys, function(key) {
            result[key] = element[key];
        });

        return result;
    };

    // Actualize request body
    this.actualizeBody = function(binding, context_path) {
        var that = this;
        var rootContext = contextPath + '.data.page';

        var fnParseBinding = function(b, s) {
            var _result = null;
            var bindingParsed = that.getBinding(b, context_path || rootContext);

            switch (bindingParsed.kind) {
                case 'binding':
                    _result = $parse(bindingParsed.value)(s);
                    break;
                case 'content':
                    _result = $interpolate(bindingParsed.value)(s);
                    break;
                case 'value':
                    _result = bindingParsed.value;
                    break;
            }

            return _result;
        };

        var fnParse = function(b, s) {
            if (that.isBinding(b)) {
                return fnParseBinding(b, s);
            } else if (angular.isArray(b)) {
                return _.map(b, function(elem) {
                    return fnParse(elem, s);
                });
            } else if (angular.isObject(b)) {
                var obj = {};

                _.each(_.pairs(b), function(elem) {
                    var key = elem[0], value = elem[1];
                    obj[key] = fnParse(value, s);
                });

                return obj;
            } else {
                return b;
            }
        };

        return fnParse(binding, context.scope);
    };

    // Create action
    this.createAction = function(container, action, name, context_path) {
        var that = this;
        var activeInstanceId = instance.active.id;
        var activeInstanceKey = instance.active.key;

        var bindingEnabled = that.getBinding(action.enabled, context_path);
        var enabled = bindingEnabled.kind === 'binding'
            ? !!$parse(bindingEnabled.value)(context.scope)
            : bindingEnabled.value === null
                ? true
                : !!$interpolate(bindingEnabled.value)(context.scope);

        if (!container.__actions.hasOwnProperty(name))
            container.__actions[name] = {
                action: action,
                contextPath: context_path,
                enabled: enabled
            };

        var actionObj = container.__actions[name];

        switch (action.type) {
            case 'request':
                // Assume valid HTTP method
                var httpMethod = 'GET';

                switch (action.method) {
                    case 'post': httpMethod = 'POST'; break;
                    case 'get': httpMethod = 'GET'; break;
                    case 'put': httpMethod = 'PUT'; break;
                    case 'delete': httpMethod = 'DELETE'; break;
                    case 'patch': httpMethod = 'PATCH'; break;
                }

                if (action.on_complete)
                    that.createAction(container, action.on_complete, name + '_on_complete', context_path);

                actionObj.fun = function() {
                    // Prepare query string values
                    var qsVals = {};

                    _.each(action.args, function(binding, param) {
                        var valueBinding = that.getBinding(binding, context_path);
                        var value = valueBinding.kind === 'binding'
                            ? $parse(valueBinding.value)(context.scope)
                            : $interpolate(valueBinding.value + '')(context.scope);

                        if (valueBinding && value) {
                            qsVals[param] = value;

                            // Check for date
                            if (angular.isDate(qsVals[param])) {
                                qsVals[param] = $filter('date')(qsVals[param], 'yyyy-MM-dd');
                            }
                        }
                    });

                    // Prepare body
                    var body = angular.isDefined(action.body) ? that.actualizeBody(action.body, context_path) : {};

                    // Do the request
                    var bindingDataUrl = that.getBinding(action.data_url, context_path);
                    var dataUrl = bindingDataUrl.kind === 'binding'
                        ? $parse(bindingDataUrl.value)(context.scope)
                        : $interpolate(bindingDataUrl.value + '')(context.scope);

                    api.requestRemoteApi(httpMethod, activeInstanceId, dataUrl, qsVals, body, function(reply) {
                        // Update data context
                        var bindingResult = that.getBinding(action.result, context_path);

                        if (bindingResult.kind === 'binding') {
                            var modelResult = $parse(bindingResult.value);
                            modelResult.assign(context.scope, utils.merge(modelResult(context.scope), reply));
                        }

                        // Trigger on_complete action
                        if (action.on_complete) {
                            $timeout(function() {
                                if (angular.isFunction(container.__actions[name + '_on_complete'].fun))
                                    container.__actions[name + '_on_complete'].fun();
                            }, 150);
                        }
                    });
                };

                break;
            case 'navigate':
                // Navigate to local or global URL
                var bindingRoute = that.getBinding(action.route, context_path);
                var route = bindingRoute.kind === 'binding'
                        ? $parse(bindingRoute.value)(context.scope)
                        : $interpolate(bindingRoute.value + '')(context.scope);

                actionObj.href = (route && route.indexOf('://') !== -1) ? route : ('/' + activeInstanceKey + '/' + route).replace(/\/+/g, '/');

                actionObj.fun = function() {
                    route = bindingRoute.kind === 'binding'
                        ? $parse(bindingRoute.value)(context.scope)
                        : $interpolate(bindingRoute.value + '')(context.scope);

                    if (route && route.indexOf('://') !== -1)
                        recent.navigateGlobal(route);
                    else
                        recent.navigate(activeInstanceKey, route.replace(/\/+/g, '/'));
                };

                break;
            case 'reload':
                // Just broadcast according events
                actionObj.fun = function() {
                    // utils.broadcast('page:load', { counter: ++counters.onLoad });
                    utils.broadcast('data:reload', { counter: ++counters.onLoad });
                };

                break;
            case 'popup':
                // Define aux data container and its path
                var dataAuxId = action.data_url ? that.generateAuxId() : '';
                context.data.aux[dataAuxId] = null;
                var contextPathDropdown = action.data_url ? [contextPath, 'data.aux', dataAuxId].join('.') : context_path;

                // Define aux container for content_template
                var contentAuxId = angular.isObject(action.content_template) ? that.generateAuxId() : '';
                var contextPathTemplate = contentAuxId ? [contextPath, 'data.aux', contentAuxId].join('.') : '';

                if (contentAuxId)
                    context.data.aux[contentAuxId] = action.content_template;

                actionObj.isContext = action.is_context;
                actionObj.dropdown_id = that.generateDropdownId();
                actionObj.dropdown = '\
                    <div class="dropdown position-absolute loader" id="%dropdown_id%"> \
                        <div class="dropdown-menu popover zeus" role="menu" prevent-click prevent-bubble style="%min_width%"> \
                            <div class="reactive-layout-stub" data-context="%context%" data-layout="%layout_url%" data-content-template="%template%"> \
                                <img src="images/other/loading.gif" id="%dropdown_id%_loader"> \
                            </div> \
                        </div> \
                    </div>'
                    .replace(/%dropdown_id%/g, actionObj.dropdown_id)
                    .replace(/%min_width%/g, action.width ? ('min-width: ' + action.width + 'px') : '')
                    .replace(/%context%/g, contextPathDropdown)
                    .replace(/%layout_url%/g, action.layout_url ? action.layout_url : '')
                    .replace(/%template%/g, contentAuxId ? contextPathTemplate : '');

                actionObj.fun = function() {
                    var modelResult = $parse(contextPathDropdown);

                    if (modelResult(context.scope) !== null)
                        return;

                    var bindingDataUrl = that.getBinding(action.data_url, context_path);
                    var dataUrl = bindingDataUrl.kind === 'binding'
                        ? $parse(bindingDataUrl.value)(context.scope)
                        : bindingDataUrl.value !== null
                            ? $interpolate(bindingDataUrl.value + '')(context.scope)
                            : null;

                    if (dataUrl) {
                        var angElement = angular.element(document.getElementById(actionObj.dropdown_id));
                        angElement.find('#' + actionObj.dropdown_id + '_loader').css({ display: 'block' });

                        api.requestRemoteApi('GET', instance.active.id, dataUrl, null, null, function(reply) {
                            modelResult.assign(context.scope, utils.merge(modelResult(context.scope), reply));
                            angElement.find('#' + actionObj.dropdown_id + '_loader').css({ display: 'none' });
                            angElement.removeClass('loader');
                        });
                    }
                };

                break;
            case 'assign':
                actionObj.fun = function() {
                    that.context.scope.$apply(function() {
                        var bindingPath = that.getBinding(action.path, context_path);
                        var bindingValue = that.getBinding(action.value, context_path);

                        if (bindingPath.kind !== 'binding')
                            return;

                        var value;

                        if ( bindingValue.kind === 'binding') {
                            value = $parse(bindingValue.value)(context.scope);
                        } else if (typeof bindingValue.value === 'boolean') {
                            value = bindingValue.value;
                        } else if (angular.isArray(bindingValue.value)) {
                            value = bindingValue.value;
                        } else if (angular.isObject(bindingValue.value) && bindingValue.value.content !== undefined) {
                            value = bindingValue.value.content;
                        } else {
                            value = $interpolate(bindingValue.value + '')(context.scope);
                        }

                        var modelPath = $parse(bindingPath.value);
                        modelPath.assign(context.scope, value);
                    });
                };

                break;
        }
    };

    // Reset reactive rendering state
    this.reset = function() {
        for (var id in elementMap) {
            if (elementMap.hasOwnProperty(id))
                angular.element(elementMap[id]).remove();
        }

        elementMap = {};
        counters = { nodes: 0, aux: 0, tabs: 0, dropdown: 0, onLoad: 0 };
    };

    // Create element container and assign event handlers
    this.createContainer = function(parent_container, element, context_path, context) {
        var elemId = this.generateNodeId();
        var div = $document[0].createElement('DIV');
        div.setAttribute('id', elemId);
        div.setAttribute('class', 'zeus-container-new');

        if (parent_container) {
            div.__contextPath = context_path;
            div.__context = context;
            div.__layout = element;
            div.__parent = parent_container;
            div.__parentContainer = parent_container;
            div.__actions = {};
            parent_container.appendChild(div);
        }

        elementMap[elemId] = div;

        return div;
    };

    this.assignBasicBehaviour = function(container, element, context_path) {
        // Get bindings
        var bindingVisible = this.getBinding(element.visible, context_path);
        var visible = (bindingVisible.value === null || bindingVisible.value === undefined) ? true : bindingVisible.value;
        var clickable = !!element.on_click;

        container.setAttribute('ng-class', ['{ hidden: !', visible.toString(), ', clickable: ', clickable.toString(), ' }'].join(''));
    };

    this.assignActions = function(container, element, context_path) {
        var events = this.getEvents(element);

        for (var eventKey in events) {
            if (events[eventKey])
                this.createAction(container, events[eventKey], eventKey, context_path);
        }
    };

    this.assignEventHandlers = function(container, element, context_path) {
        // Get container element
        var elemContainer = angular.element(container);

        // Get action descriptor objects
        var onLoad = container.__actions.on_load;
        var onHover = container.__actions.on_hover;
        var onClick = container.__actions.on_click;
        var onChange = container.__actions.on_change;

        // Attach on_click event handler
        elemContainer.on('mousedown', function(e) {
            if (onClick && onClick.enabled && e.which === 1) {
                onClick.fun();
            }
        });

        // Attach on_hover event handler
        elemContainer.on('mouseenter', function(e) {
            if (onHover && onHover.enabled) {
                onHover.fun();
            } else {
                var options = elemContainer.data('contextMenuOptions');

                if (options) {
                    var contextMenuOptions = angular.fromJson(elemContainer.data('contextMenuOptions').replace(/'/g, '"'));

                    if (contextMenuOptions.hover.target !== 'null') {
                        var popupHover = document.getElementById(contextMenuOptions.hover.target);
                        popupHover.style.display = 'none';
                    }
                }
            }
        });

        // Attach on_change event handler
        elemContainer.on('zeus:on:change', function() {
            $timeout(function() {
                if (onChange && onChange.enabled)
                    onChange.fun();
            }, 50);
        });

        // Attach on load event handler
        if (onLoad && onLoad.enabled) {
            onLoad.updateCounter = 0;
            context.scope.$on('page:load', function($event, opts) {
                if (opts.counter > onLoad.updateCounter) {
                    onLoad.updateCounter = opts.counter;
                    onLoad.fun();
                }
            });
        }

        context.scope.$on('$destroy', function() {
            elemContainer.off('mousedown');
            elemContainer.off('zeus:on:change');
        });
    };

    this.assignDropdowns = function(parent_container, container) {
        // Handle on_click dropdown
        var onClick = angular.isObject(container.__actions.on_click) ? container.__actions.on_click : null;
        var dropdownId = (onClick && onClick.dropdown_id) ? onClick.dropdown_id : 'null';
        var dropdown = (onClick && onClick.dropdown) ? onClick.dropdown : '';
        var dropdownStyle = (onClick && !onClick.isContext) ? 'popover' : 'popup';

        // Handle on_hover dropdown
        var onHover = angular.isObject(container.__actions.on_hover) ? container.__actions.on_hover : null;
        var hoverdownId = (onHover && onHover.dropdown_id) ? onHover.dropdown_id : 'null';
        var hoverdown = (onHover && onHover.dropdown) ? onHover.dropdown : '';
        var hoverdownStyle = (onHover && !onHover.isContext) ? 'popover' : 'popup';

        if (dropdown || hoverdown) {
            var contextMenuOptions = {
                click: { target: dropdownId, style: dropdownStyle },
                hover: { target: hoverdownId, style: hoverdownStyle }
            };

            container.setAttribute('context-menu', true);
            container.setAttribute('data-context-menu-options', JSON.stringify(contextMenuOptions).replace(/\"/g, "'"));
        }

        // Append dropdown
        if (dropdown) {
            var elemDropdown = angular.element(dropdown);
            parent_container.appendChild(elemDropdown[0]);
        }

        // Append hoverdown
        if (hoverdown) {
            var elemHoverdown = angular.element(hoverdown);
            parent_container.appendChild(elemHoverdown[0]);
        }
    };

    // Add custom element renderer
    this.addRenderer = function(element_kind, fun) {
        if (element_kind === 'unknown')
            throw 'Cannot add handler to element type "unknown"';

        renderers[element_kind] = fun;
    };

    // Entry point for dynamic content rendering
    this.render = function(parent_container, element, context_path, context) {
        var that = this;

        // Add element and assign behaviour
        var elemParent = angular.element(parent_container);
        var containerBlock = this.createContainer(parent_container, element, context_path, context);
        var elemContainer = angular.element(containerBlock);
        this.assignBasicBehaviour(containerBlock, element, context_path);
        this.assignActions(containerBlock, element, context_path);
        this.assignEventHandlers(containerBlock, element, context_path);
        this.assignDropdowns(parent_container, containerBlock);

        elemContainer.on('mousemove', function() {
            if (!containerBlock.__dropdownsRendered) {
                containerBlock.__dropdownsRendered = true;

                // Check for dropdowns
                var onClick = angular.isObject(containerBlock.__actions.on_click) ? containerBlock.__actions.on_click : null;
                var dropdown = (onClick && onClick.dropdown) ? onClick.dropdown : null;

                var onHover = angular.isObject(containerBlock.__actions.on_hover) ? containerBlock.__actions.on_hover : null;
                var hoverdown = (onHover && onHover.dropdown) ? onHover.dropdown : null;

                if (dropdown || hoverdown) {
                    var batch = angular.element('.reactive-layout-stub', elemParent).attr('reactive-layout', true);
                    $compile(batch)(that.context.scope);
                }
            }
        });

        // Run custom renderer
        var fnRender = angular.isFunction(renderers[element.type]) ? renderers[element.type] : renderers['unknown'];
        fnRender(containerBlock, element, context_path, context);

        return containerBlock;
    };
});
