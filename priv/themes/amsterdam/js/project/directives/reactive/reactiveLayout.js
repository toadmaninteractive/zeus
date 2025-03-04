angular.module('zeusApp').directive('reactiveLayout', function($compile, $parse, $timeout, storage, instance, reactive, utils) {
    return {
        restrict: 'A',
        link: function(scope, element) {
            // Initialize context
            var useScope;
            var contextPath = element.data('context');
            var modelContext = $parse(contextPath);
            var layoutUrl = element.data('layout');
            var contentTemplatePath = element.data('content-template');
            var contentTemplate = $parse(contentTemplatePath)(scope);
            var defaultContent = '<!-- -->';

            var fnRetrieveAndRenderLayout = function(layout_info) {
                // Replace element contents
                element.stop(true, true).fadeOut('fast', function() {
                    angular.isDefined(useScope) && useScope.$destroy();
                    useScope = scope.$new(false, scope);
                    element.empty();

                    // Compile new content
                    if (!layout_info || angular.isString(layout_info))
                        element.html(layout_info || defaultContent);
                    else
                        reactive.render(element[0], layout_info.data.layout, contextPath, modelContext(scope));

                    $compile(element.contents())(useScope);

                    $timeout(function() {
                        utils.broadcast('page:load', { counter: reactive.counters.onLoad });
                    }, 100);

                    element.fadeIn('fast');
                });
            };

            var fnRenderContentTemplate = function(content_template) {
                angular.isDefined(useScope) && useScope.$destroy();
                useScope = scope.$new(false, scope);
                element.empty();

                // Compile new content
                reactive.render(element[0], content_template, contextPath, modelContext(scope));
                $compile(element.contents())(useScope);

                $timeout(function() {
                    utils.broadcast('page:load', { counter: reactive.counters.onLoad });
                }, 100);
            };

            var rendered = false;

            scope.$watch(function() {
                return modelContext(scope);
            }, function(new_val, old_val) {
                if (!rendered && new_val !== null) {
                    rendered = true;

                    if (angular.isString(layoutUrl) && layoutUrl.length > 0 && instance.active.id) {
                        // Request layout & render it
                        storage.getLayout(instance.active.id, layoutUrl, fnRetrieveAndRenderLayout);
                    } else if (angular.isString(contentTemplatePath) && contentTemplatePath.length > 0) {
                        // Render content template
                        fnRenderContentTemplate(contentTemplate);
                    } else {
                        // Render default content
                        fnRetrieveAndRenderLayout(defaultContent);
                    }
                }
            }, true);

            // HACK: this magic ensures layout to be rendered, somehow...
            if (angular.isObject(modelContext(scope)))
                $timeout(angular.identity, 0);
        }
    }
});
