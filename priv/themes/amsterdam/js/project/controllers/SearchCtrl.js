angular.module('zeusApp').controller('SearchCtrl', [
    '$scope', 'api', 'instance', 'utils', 'growl', 'recent',
    function($scope, api, instance, utils, growl, recent) {
        $scope.opts = { input: '' };

        $scope.handleInput = function($event) {
            if ($event.keyCode === 27) {
                $scope.opts.input = '';
                utils.broadcast('toggle:search');
            }
        };

        $scope.search = function() {
            if (!instance.active.id)
                return;

            api.remoteSearch(instance.active.id, $scope.opts.input, function(reply) {
                if (reply.error) {
                    growl.message(reply.error, 'Search failed', 'warning', 2500);
                } else if (angular.isString(reply.route)) {
                    $scope.opts.input = '';
                    utils.broadcast('toggle:search');
                    recent.navigate(instance.active.key, reply.route);
                }
            });
        };
    }
]);
