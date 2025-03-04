angular.module('zeusApp').controller('RecentPagesCtrl', [
    '$scope', 'recent',
    function($scope, recent) {
        $scope.recent = recent;

        $scope.getIconUrl = function(page) {
            return page && page.instance && page.instance.agentInfo && page.instance.agentInfo.icon
                ? ['_utils/api/instances/link/',page.instance.serverInfo.id, '/', page.instance.agentInfo.icon, '?', page.tick].join('')
                : 'images/icons/projects/_unknown.png';
        };

        $scope.combineUrl = function(route, search) {
            var part = search ? ('?' + search) : '';

            return route + part;
        };
    }
]);
