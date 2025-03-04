angular.module('zeusApp').config(function($provide) {
    $provide.decorator('reactive', function($delegate, $parse, $interpolate, $location, $websocket, api, utils, instance) {
        $delegate.addRenderer('tabs', function(parent_container, element, context_path, context) {
            // Update parent container style
            parent_container.style.display = 'block';

            // Create tab selected page
            var tabPageId = $delegate.generateAuxId();
            var tabPageContextPath = [$delegate.contextPath, 'data.aux', tabPageId].join('.');
            $delegate.context.data.aux[tabPageId] = {};
            var tabPageContext = $parse(tabPageContextPath)($delegate.context.scope);

            // Create tabs box
            var elemTabs = angular.element('<div class="tabbable page-tabs instance"></div>');
            parent_container.appendChild(elemTabs[0]);

            // Add tab headers container
            var elemTabHeaders = angular.element('<ul class="nav nav-tabs"></ul>');
            elemTabs[0].appendChild(elemTabHeaders[0]);

            // Add tab pages container
            var elemTabPages = angular.element('<div class="tab-content zeus"></div>');
            elemTabs[0].appendChild(elemTabPages[0]);

            // Define templates
            var templateHeader = '\
                <li class="%active%"> \
                    <a \
                        href="javascript:void(null)" \
                        data-target="#%tab_id%" \
                        data-toggle="tab" \
                        onclick="window.location.hash = \'#%tab_id%\'" \
                        ng-click="%tab_page_context%._tab = \'%tab_id%\'; %tab_page_context%.%tab_id% = true"> \
                            <i class="%icon%"></i> \
                            %title% \
                    </a> \
                </li>';

            var templatePage = '\
                <div class="tab-pane fade %active%" id="%tab_id%"> \
                    <div \
                        reactive-layout \
                        data-context="%page_context%" \
                        data-layout="%layout%" \
                        ng-if="%tab_page_context%._tab === \'%tab_id%\' || %tab_page_context%.%tab_id%"> \
                            <img src="images/other/loading.gif" /> \
                    </div> \
                </div>';

            // Get hash
            var activeTabId = +($location.hash().split('_')[1] || 0);

            // Add tabs
            _.each(element.items, function(item) {
                // Define tab and aux IDs and page context
                var tabId = $delegate.generateTabId();
                var auxId = item.data_url ? $delegate.generateAuxId() : '';
                var pageContext = item.data_url ? [$delegate.contextPath, 'data.aux', auxId].join('.') : context_path;

                // Check selected page
                if (!tabPageContext._tab) {
                    tabPageContext._tab = activeTabId;
                    tabPageContext[activeTabId] = true;
                    setTimeout(() => $(`a[data-target='#tab_${activeTabId}'`).click(), 100);
                }

                // Assign empty aux data set
                if (auxId)
                    $delegate.context.data.aux[auxId] = [];

                // Create tab header
                var header = templateHeader
                    .replace(/%active%/g, element.items.indexOf(item) === activeTabId ? 'active' : '')
                    .replace(/%tab_id%/g, tabId)
                    .replace(/%tab_page_context%/g, tabPageContextPath)
                    .replace(/%icon%/g, item.icon || '')
                    .replace(/%title%/g, item.title || '');

                var elemHeader = angular.element(header);

                // Create tab page
                var page = templatePage
                    .replace(/%active%/g, element.items.indexOf(item) === activeTabId ? 'active in' : '')
                    .replace(/%tab_id%/g, tabId)
                    .replace(/%tab_page_context%/g, tabPageContextPath)
                    .replace(/%page_context%/g, pageContext)
                    .replace(/%layout%/g, item.layout_url || 'undefined');

                var elemPage = angular.element(page);

                // Add header and page
                elemTabHeaders[0].appendChild(elemHeader[0]);
                elemTabPages[0].appendChild(elemPage[0]);

                item.handledOnLoad = false;

                // Add listener to page:load event
                $delegate.context.scope.$on('page:load', function($event, opts) {
                    if (item.handledOnLoad)
                        return;

                    item.handledOnLoad = true;
                    var dataUrl = $delegate.contentInterpolate(item.data_url || '', context_path);

                    /*
                    // OLD
                    if (dataUrl !== '') {
                        dataUrl = $interpolate(dataUrl)($delegate.context.scope);

                        api.requestRemoteApi('GET', instance.active.id, dataUrl, null, null, function(reply) {
                            utils.merge($delegate.context.data.aux[auxId], reply);
                            utils.broadcast('page:load', opts.counter);
                        });
                    }
                    */

                    if (dataUrl == '')
                        return;

                    dataUrl = $interpolate(dataUrl)($delegate.context.scope);

                    if (item.is_websocket) {
                        // WebSocket data source:
                        // - Assign data source on connect
                        // - Update data source using JSON patch method
                        var wsUrl = utils.makeWebsocketUrl($delegate.context.pageInfo.instance_id, dataUrl);
                        var wsInitialized = false;
                        var websocket = $websocket(wsUrl);
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

                                    if (angular.isArray($delegate.context.data.aux[auxId]) && !angular.isArray(json))
                                        $delegate.context.data.aux[auxId] = json;
                                    else
                                        utils.merge($delegate.context.data.aux[auxId], json);
                                } else {
                                    // Apply json patch
                                    var patched = jsonpatch.apply_patch($delegate.context.data.aux[auxId], json);
                                    utils.merge($delegate.context.data.aux[auxId], patched);
                                }
                            }, { autoApply: true });

                        $delegate.context.scope.$on('$destroy', function() {
                            if (websocket) {
                                websocket.close();
                                websocket = null;
                            }
                        });
                    } else {
                        api.requestRemoteApi('GET', instance.active.id, dataUrl, null, null, function(reply) {
                            utils.merge($delegate.context.data.aux[auxId], reply);
                            utils.broadcast('page:load', opts.counter);
                        });
                    }
                });
            });
        });

        return $delegate;
    });
});
