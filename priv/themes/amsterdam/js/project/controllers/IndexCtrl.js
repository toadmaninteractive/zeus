angular.module('zeusApp').controller('IndexCtrl', [
    '$scope', '$window', '$location', '$templateCache', '$cookieStore', 'RoutingSchema', 'NgTables', 'api', 'instance', 'recent', 'storage', 'utils',
    function($scope, $window, $location, $templateCache, $cookieStore, RoutingSchema, NgTables, api, instance, recent, storage, utils) {
        // Constants
        $scope.RoutingSchema = RoutingSchema;
        $scope.NgTables = NgTables;

        // Services
        $scope.$location = $location;
        $scope.api = api;
        $scope.utils = utils;
        $scope.instance = instance;

        // Authentication state
        $scope.username = '';
        $scope.realm = '';
        $scope.is_root = false;
        $scope.roles = { admin: [], user: {} };
        $scope.authenticated = false;
        $scope.stateInitialized = false;
        $scope.version = '';
        $scope.window = { width: $window.innerWidth, height: $window.innerHeight };

        // Active templates
        $scope.templateIndex = '';
        $scope.templateLayout = '';

        // Initialize controller
        $scope.init = function() {
            // Replace ng-tables default templates
            _.each(NgTables, function(template, key) {
                $templateCache.put(key, template);
            });

            // Get zeus version
            api.version(function(reply) {
                if (reply.result !== false)
                    $scope.version = reply.version || '';
            });

            // Check authentication state
            api.state(function(reply) {
                $scope.stateInitialized = true;
                $scope.authenticated = reply.result;
                $scope.username = reply.username || '';
                $scope.realm = reply.realm || '';
                $scope.is_root = reply.is_root || false;
                $scope.roles = utils.merge($scope.roles, { admin: reply.admin_roles || [], user: reply.user_roles || {} });
                $scope.updateLayout();
            });
        };

        // Layout content manipulation
        $scope.updateLayout = function() {
            var path = $location.path();
            var router = $scope.authenticated ? $scope.RoutingSchema.authenticated : $scope.RoutingSchema.not_authenticated;
            var route = _.find(router.routes, function(obj) { return new RegExp(obj.regexp, 'g').test(path); }) || router.otherwise;

            if (route.redirectTo) {
                if (!$scope.authenticated && path !== route.redirectTo)
                    $cookieStore.put('redirectAfterLoginUrl', path);

                $location.path(route.redirectTo);
            } else {
                var oldIndex = $scope.templateIndex;
                var oldLayout = $scope.templateLayout;

                $scope.templateIndex = route.index || '';
                $scope.templateLayout = route.layout || '';

                ($scope.templateIndex !== oldIndex) && utils.broadcast('scope:change', { subject: 'index' });
                ($scope.templateLayout !== oldLayout) && utils.broadcast('scope:change', { subject: 'layout' });
            }
        };

        // Watch event: $locationChangeSuccess
        $scope.$on('$locationChangeSuccess', function(event, data) {
            if ($scope.stateInitialized)
                $scope.updateLayout();
        });

        // Watch event: user:login
        $scope.$on('user:login', function(event, data) {
            $scope.authenticated = true;
            $scope.username = data.username || '';
            $scope.realm = data.realm || '';
            $scope.is_root = data.is_root || false;
            $scope.roles.admin = data.admin_roles;
            $scope.roles.user = data.user_roles;

            var redirectAfterLoginUrl = $cookieStore.get('redirectAfterLoginUrl');

            if (redirectAfterLoginUrl) {
                $cookieStore.remove('redirectAfterLoginUrl');
                $location.path(redirectAfterLoginUrl);
            } else {
                $location.path('/admin/dashboard');
            }
        });

        // Watch event: user:logout
        $scope.$on('user:logout', function() {
            recent.clear();
            storage.clear();
            instance.items.list.clear();
            $scope.authenticated = false;
            $scope.username = '';
            $scope.realm = '';
            $scope.is_root = false;
            $scope.roles.admin.length = 0;
            $scope.roles.user.length = 0;
            $location.path('/admin/login');
        });

        $scope.$on('api:not_authenticated', function() {
            if ($scope.authenticated) {
                $cookieStore.put('redirectAfterLoginUrl', $location.path());
                utils.broadcast('user:logout');
            }
        });

        angular.element($window).bind('resize', function() {
            $scope.$apply(function() {
                $scope.window.width = $window.innerWidth;
                $scope.window.height = $window.innerHeight;
            });
        });

        $scope.init();
    }
]);
