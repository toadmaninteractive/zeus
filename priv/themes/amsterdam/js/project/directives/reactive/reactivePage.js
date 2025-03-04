angular.module('zeusApp').directive('reactivePage', function($compile, $parse, $interpolate, $timeout, $q, $websocket, api, utils, reactive, storage) {
    return {
        restrict: 'A',
        link: function(scope, element) {
            // Workflow:
            // 1. Initialize
            //    - Get context path
            //    - Get context from scope
            //    - Verify context: page info, data (throw error in case of invalid data)
            //    - Assign context to reactive service
            //    - Define update procedure
            // 2. Add watch for page information
            //    - On update & valid page information -> request data and render page
            // 3. Attach event handlers
            //    - data:reload -> request data and render page
            //    - page:reload -> request data and render page (was: request data)

            // Initialize context
            var contextPath = element.data('context');
            var context = $parse(contextPath)(scope);
            var updateAttempt = 1;
            var websocket = null;

            // Verify context
            if (!angular.isObject(context))
                throw 'Invalid context: not an object';
            else if (!angular.isFunction(context.convert))
                throw 'Invalid context: convert is not a function';
            else if (!angular.isObject(context.pageInfo))
                throw 'Invalid context: pageInfo is not an object';
            else if (!angular.isObject(context.data))
                throw 'Invalid context: data is not an object: data';

            // Assign context
            reactive.assignContext(context, contextPath);

            // Define update function: get layout and data, render content
            var fnUpdate = function(attempt) {
                // Update only if page information is valid
                if (!context.pageInfo.isValid())
                    return;

                // Re-create scope and clear content, close websocket if exists
                context.scope && context.scope.$destroy();

                if (websocket) {
                    websocket.close();
                    websocket = null;
                }

                context.scope = scope.$new(false, scope);
                reactive.reset();

                // Emit event: page:change start
                utils.broadcast('page:change', true);

                // Get page layout
                var defLayout = $q.defer();

                if (reactive.isInternalRoute(context.pageInfo.layout_url)) {
                    defLayout.resolve({ internal: true });
                } else {
                    storage.getLayout(context.pageInfo.instance_id, context.pageInfo.layout_url, function(reply) {
                        var result = reply.result === false ? reply : reply.result;
                        defLayout.resolve(result);
                    });
                }

                // Reset auxiliary data
                utils.merge(context.data.aux, {});

                // Get data
                var defData = $q.defer();
                var pageData = context.pageInfo.data || {};

                if (context.pageInfo.data_url) {
                    if (context.pageInfo.is_websocket) {
                        // WebSocket data source:
                        // - Assign data source on connect
                        // - Update data source using JSON patch method
                        var wsUrl = utils.makeWebsocketUrl(scope.pageInfo.instance_id, context.pageInfo.data_url);
                        var wsInitialized = false;
                        websocket = $websocket(wsUrl);
                        websocket
                            .onOpen(function(ev) {})
                            .onClose(function(ev) {})
                            .onError(function(error) {})
                            .onMessage(function(msgEv) {
                                var json = utils.json(msgEv.data, null);

                                if (!(angular.isObject(json) || angular.isArray(json)))
                                    return;

                                if (!wsInitialized) {
                                    wsInitialized = true;
                                    utils.append(pageData, json);
                                    defData.resolve(pageData || {});
                                } else {
                                    // Apply json patch
                                    var patched = jsonpatch.apply_patch(context.data.page, json);
                                    utils.merge(context.data.page, patched);
                                }
                            }, { autoApply: true });
                    } else {
                        // Regular data source
                        api.requestRemoteApi('GET', scope.pageInfo.instance_id, context.pageInfo.data_url, null, null, function(reply) {
                            utils.append(reply, pageData);
                            defData.resolve(reply || {});
                        });
                    }
                } else {
                    defData.resolve(pageData);
                }

                // Render content when promises are resolved
                $q.all([defLayout.promise, defData.promise]).then(function(result) {
                    if (attempt < updateAttempt)
                        return;

                    // Store data
                    if (result[1].result !== false) {
                        if (angular.isArray(context.data.page) && !angular.isArray(result[1]))
                            context.data.page = result[1];
                        else
                            utils.merge(context.data.page, result[1]);
                    }

                    // Render and compile content
                    if (!result[0].internal) {
                        if (result[0].result === false) {
                            var templError = '<span class="label label-important">Failed to get page layout. It seems Zeus Agent is offline or has invalid set up.</span>';
                            element.html(templError);
                        } else {
                            element[0].innerHTML = '';
                            reactive.render(element[0], result[0], contextPath + '.data.page', context);
                            $compile(element.contents())(context.scope);
                        }
                    }

                    // Emit event: page:change finish
                    utils.broadcast('page:change', false);

                    // Emit page load notification
                    $timeout(function() {
                        utils.broadcast('page:load', { counter: ++reactive.counters.onLoad });
                    }, 100);
                });
            };

            // Monitor page information change
            scope.$watch(function() {
                return scope.pageInfo;
            }, function(new_value, old_value) {
                if (!angular.equals(new_value, old_value))
                    fnUpdate();
            }, true);

            // Handle data:reload event
            scope.$on('data:reload', function() {
                fnUpdate(++updateAttempt);
            });

            // Handle page:reload event
            scope.$on('page:reload', function() {
                fnUpdate(++updateAttempt);
            });

            scope.$on('$destroy', function() {
                if (websocket) {
                    websocket.close();
                    websocket = null;
                }
            });
        }
    }
});
