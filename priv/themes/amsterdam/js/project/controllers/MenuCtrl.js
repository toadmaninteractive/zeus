angular.module('zeusApp').controller('MenuCtrl', [
    '$scope', '$location', 'Menu', 'instance', 'recent',
    function($scope, $location, Menu, instance, recent) {
        $scope.Menu = Menu;
        $scope.instance = instance;
        $scope.recent = recent;

        $scope.isMenuActive = function(item) {
            return new RegExp(item.regexp, 'g').test($location.path());
        };

        $scope.hasActiveChildren = function(item) {
            var hasActive = false;

            var fnCheck = function(el) {
                if (hasActive = hasActive || el.itemInfo.route === instance.active.route)
                    return;

                _.each(el.items, fnCheck);
            };

            fnCheck(item);

            return hasActive;
        };

        $scope.hasAccessToMenu = function(item) {
            if ($scope.is_root || !angular.isObject(item.restrict))
                return true;

            if (angular.isArray(item.restrict.admin))
                return angular.equals(_.intersection(item.restrict.admin, $scope.roles.admin), item.restrict.admin);
            else if (item.restrict.admin === true)
                return $scope.roles.admin.length > 0;
            else
                return false;
        };

        $scope.hasAccessToAdmin = function(instance_id) {
            if ($scope.is_root || $scope.roles.admin.indexOf('manage_instances') !== -1)
                return true;

            var userRoles = $scope.roles.user[instance_id] ? $scope.roles.user[instance_id] : [];

            return userRoles.indexOf('admin') !== -1;
        };

        $scope.hasAccessToLogs = function(instance_id) {
            if ($scope.is_root || $scope.roles.admin.indexOf('manage_instances') !== -1)
                return true;

            return $scope.roles.user[instance_id] ? $scope.roles.user[instance_id].length > 0 : false;
        };

        $scope.hasAccessToInstance = function(item) {
            var isAuthorizedUser = $scope.roles.user[item.serverInfo.id] && $scope.roles.user[item.serverInfo.id].length > 0;

            if ($scope.is_root || $scope.roles.admin.indexOf('manage_instances') !== -1 || isAuthorizedUser)
                return true;
        };
    }
]);
