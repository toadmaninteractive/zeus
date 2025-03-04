angular.module('zeusApp').service('recent', function($window, $location, instance, storage, utils, api) {
    var pages = [];

    this.getPages = function(number) {
        return (!number || pages.length <= number)
            ? pages
            : pages.slice(pages.length - number, pages.length);
    };

    this.addPage = function(instance_key, route, search) {
        var url = ['', instance_key, route].join('/').replace(/[\/]+/g, '/');
        var existingItem = _.findWhere(pages, { instance_key: instance_key, route: route, url: url });

        if (existingItem && pages.indexOf(existingItem) === pages.length - 1)
            return;

        if (existingItem) {
            // Splice and relocate
            var index = _.indexOf(pages, existingItem);
            pages.splice(index, 1);
            pages.push(existingItem)
        } else {
            // Add new
            var obj = {
                instance_key: instance_key,
                route: route,
                search: search,
                url: url,
                instance: instance.helpers.itemByKey(instance_key),
                tick: utils.randomString()
            };

            if (route === '/_admin') {
                obj.pageInfo = {title: 'Administration'};
            } else if (route === '/_logs') {
                obj.pageInfo = { title: 'Action Logs' };
            } else {
                storage.getPageInfo(obj.instance.serverInfo.id, route, function(reply) {
                    obj.pageInfo = reply;
                });
            }

            pages.push(obj)
        }
    };

    this.clear = function() {
        pages.length = [];
    };

    this.navigate = function(instance_key, route) {
        var attachmentTag = '/_attachment';
        var url = ['', instance_key, route].join('/').replace(/[\/]+/g, '/').replace(/\/+$/, '');

        // Encode URL
        url = encodeURI(url);

        var isDataReloadRequired = $location.path() === url.split('?')[0];

        if (route.indexOf(attachmentTag + '/') === 0) {
            var item = instance.helpers.itemByKey(instance_key);
            var path = api.getInstanceUrl(item.serverInfo.id, route.substr(attachmentTag.length));
            $window.open(path);
        } else if (url !== $location.path()) {
            var routeParts = route.split('?');
            var routePath = routeParts[0];
            var routeSearch = routeParts[1] || '';

            if (routePath !== '/_logs') {
                $location.$$search = {};
            }

            this.addPage(instance_key, routePath, routeSearch);
            $location.url(url);

            if (isDataReloadRequired) {
                // 1. AngularJS does not reload controller when path is the same -> trigger data:reload event instead
                // 2. Trigger data:reload event with a delay to make sure route change is processed
                setTimeout(() => utils.broadcast('data:reload'), 100);
            }
        }
    };

    this.navigateGlobal = function(url) {
        $window.open(url);
    };
});
