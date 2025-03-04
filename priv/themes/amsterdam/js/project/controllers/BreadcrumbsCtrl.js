angular.module('zeusApp').controller('BreadcrumbsCtrl', [
    '$scope', '$location', 'Breadcrumbs', 'instance', 'storage',
    function($scope, $location, Breadcrumbs, instance, storage) {
        $scope.breadcrumbs = [];

        $scope.getBreadcrumbs = function() {
            var crumbs = angular.copy(Breadcrumbs);

            var entry = _.find(crumbs, function(obj) {
                return new RegExp(obj.regexp, 'g').test($location.path());
            });

            // Update remote
            var length = entry ? entry.items.length : 0;

            if (length > 0 && entry.items[length - 1].remote && instance.active.id) {
                entry.items[length - 1].title = instance.active.item
                    ? instance.active.item.serverInfo.title
                    : '[Unknown instance]';

                entry.items[length - 1].url = '/' + instance.active.key;

                if (instance.active.route === '/_admin') {
                    entry.items.push({
                        title: 'Administration',
                        url: ['', instance.active.key, '_admin'].join('/')
                    });
                } else if (instance.active.route === '/_logs') {
                    entry.items.push({
                        title: 'Action Logs',
                        url: ['', instance.active.key, '_logs'].join('/')
                    });
                } else {
                    storage.getPageInfo(instance.active.id, instance.active.route, function (replyPage) {
                        entry.items.push({
                            title: replyPage.title || '[Unknown page: %page%]'.replace(/%page%/g, instance.active.route || ''),
                            url: instance.active.route || ''
                        });
                    });
                }
            }

            return entry.items || [];
        };

        $scope.$watch(function() {
            return instance.active.item;
        }, function() {
            $scope.breadcrumbs = $scope.getBreadcrumbs();
        }, true);

        $scope.$on('$locationChangeSuccess', function() {
            $scope.breadcrumbs = $scope.getBreadcrumbs();
        });

        $scope.getBreadcrumbs();
    }
]);
