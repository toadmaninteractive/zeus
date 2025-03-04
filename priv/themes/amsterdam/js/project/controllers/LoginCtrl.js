angular.module('zeusApp').controller('LoginCtrl', [
    '$scope', '$timeout', 'api', 'utils', 'growl',
    function($scope, $timeout, api, utils, growl) {
        $scope.authRealms = [];
        $scope.selectedRealm = null;

        $scope.init = function() {
            // Get authentication state
            api.realms(function(reply) {
                $scope.authRealms = reply;
                $scope.selectedRealm = (angular.isArray(reply) && reply.length > 0) ? reply[0] : undefined;
            });
        };

        $scope.login = function() {
            var username = angular.element('input[name="username"]').val();
            var password = angular.element('input[name="password"]').val();

            api.login(username, password, $scope.selectedRealm.realm, function(reply) {
                if (reply.result) {
                    utils.broadcast('user:login', reply);
                } else {
                    growl.message(reply.error, 'Authentication failed', 'error', 3000);
                    utils.broadcast('resetForm');
                    username = '';
                    password = '';
                }
            });
        };

        $scope.logout = function() {
            api.logout(function(reply) {
                if (reply.result)
                    utils.broadcast('user:logout');
            });
        };

        $scope.init();
    }
]);
