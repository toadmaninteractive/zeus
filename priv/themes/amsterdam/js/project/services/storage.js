angular.module('zeusApp').service('storage', function($http, api) {
    var localStorage;

    this.getTemplate = function(template_name, callback) {
        if (localStorage.templates[template_name] && localStorage.templates[template_name].status === 'ready') {
            angular.isFunction(callback) && callback(localStorage.templates[template_name].info);
        } else if (localStorage.templates[template_name] && localStorage.templates[template_name].status === 'in_progress') {
            angular.isFunction(callback) && localStorage.templates[template_name].notify_list.push(callback);
        } else {
            var template = {
                status: 'in_progress',
                notify_list: []
            };

            localStorage.templates[template_name] = template;
            angular.isFunction(callback) && template.notify_list.push(callback);

            api.localTemplate(template_name, function(reply) {
                template.info = reply;
                template.status = 'ready';

                _.each(template.notify_list, function(notifier) {
                    notifier(template.info);
                });

                template.notify_list.length = 0;
            });
        }
    };

    this.getLayout = function(instance_id, layout_url, callback) {
        var layoutKey = instance_id + '::' + layout_url;

        if (localStorage.layouts[layoutKey] && localStorage.layouts[layoutKey].status === 'ready') {
            angular.isFunction(callback) && callback(localStorage.layouts[layoutKey].info);
        } else if (localStorage.layouts[layoutKey] && localStorage.layouts[layoutKey].status === 'in_progress') {
            angular.isFunction(callback) && localStorage.layouts[layoutKey].notify_list.push(callback);
        } else {
            var layout = {
                status: 'in_progress',
                notify_list: []
            };

            localStorage.layouts[layoutKey] = layout;
            angular.isFunction(callback) && layout.notify_list.push(callback);

            api.remoteLayout(instance_id, layout_url, function(reply) {
                layout.info = {result: !reply.error && reply.element, data: { layout: reply.element }};
                layout.status = 'ready';

                _.each(layout.notify_list, function(notifier) {
                    notifier(layout.info);
                });

                layout.notify_list.length = 0;
            });
        }
    };

    this.getPageInfo = function(instance_id, page_url, callback) {
        if (!instance_id)
            return;

        var pageKey = instance_id + '::' + page_url;

        if (localStorage.pages[pageKey] && localStorage.pages[pageKey].status === 'ready') {
            angular.isFunction(callback) && callback(localStorage.pages[pageKey].info);
        } else if (localStorage.pages[pageKey] && localStorage.pages[pageKey].status === 'in_progress') {
            angular.isFunction(callback) && localStorage.pages[pageKey].notify_list.push(callback);
        } else {
            var page = {
                status: 'in_progress',
                notify_list: []
            };

            localStorage.pages[pageKey] = page;

            if (page_url === '/_admin') {
                page.status = 'ready';
                page.info = {title: 'Administration', icon: '', layout_url: '', data_url: ''};
                angular.isFunction(callback) && callback(localStorage.pages[pageKey].info);
            } else if (page_url === '/_logs') {
                page.status = 'ready';
                page.info = { title: 'Action Logs', icon: '', layout_url: '', data_url: '' };
                angular.isFunction(callback) && callback(localStorage.pages[pageKey].info);
            } else {
                angular.isFunction(callback) && page.notify_list.push(callback);

                api.remotePage(instance_id, page_url, function (reply) {
                    page.info = reply;
                    page.status = 'ready';

                    _.each(page.notify_list, function (notifier) {
                        notifier(page.info);
                    });

                    page.notify_list.length = 0;
                });
            }
        }
    };

    this.clear = function() {
        localStorage = { templates: {}, layouts: {}, pages: {} };
    };

    this.clear();
});
