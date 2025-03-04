angular.module('zeusApp').controller('UsersViewCtrl', [
    '$scope', '$filter', '$modal', '$q', 'ngTableParams', 'api', 'instance', 'dialogs', 'growl', 'utils',
    function($scope, $filter, $modal, $q, ngTableParams, api, instance, dialogs, growl, utils) {
        // Store services
        $scope.utils = utils;

        // User data
        $scope.baseUrl = '/auth/users';
        $scope.realms = [];
        $scope.realmMap = {};
        $scope.users = [];

        // Attach realm functions
        $scope.realms.read = function() {
            api.realms(function(reply) {
                if (angular.isArray(reply)) {
                    $scope.realms = utils.merge($scope.realms, reply);

                    $scope.realmMap = {};

                    _.each($scope.realms, function(item) {
                        $scope.realmMap[item.realm] = item;
                    });
                }
            });
        };

        // Attach user functions
        $scope.users.create = function(user) {
            var deferred = $q.defer();

            api.request('POST', $scope.baseUrl, {}, user, function(reply) {
                if (reply.result !== false)
                    $scope.users.push(user);

                deferred.resolve(reply);
            });

            return deferred.promise;
        };

        $scope.users.read = function() {
            var deferred = $q.defer();

            api.request('GET', $scope.baseUrl, {}, {}, function(reply) {
                if (angular.isArray(reply))
                    $scope.users = utils.merge($scope.users, reply);

                deferred.resolve(reply);
            });

            return deferred.promise;
        };

        $scope.users.update = function(user) {
            var deferred = $q.defer();

            api.request('PUT', [$scope.baseUrl, '/', user.realm, '/', user.login].join(''), {}, user, function(reply) {
                if (reply.result !== false) {
                    var found = _.findWhere($scope.users, { login: user.login, realm: user.realm });
                    utils.merge(found, user);
                }

                deferred.resolve(reply);
            });

            return deferred.promise;
        };

        $scope.users.delete = function(user) {
            var deferred = $q.defer();

            var url = [$scope.baseUrl, '/', user.realm, '/', user.login].join('');

            api.request('DELETE', url, {}, {}, function(reply) {
                if (reply.result !== false) {
                    var found = _.findWhere($scope.users, { login: user.login, realm: user.realm });

                    if (found)
                        $scope.users.splice($scope.users.indexOf(found), 1);
                }

                deferred.resolve(reply);
            });

            return deferred.promise;
        };

        // Watch user entries
        $scope.$watch(function() {
            return $scope.users;
        }, function() {
            $scope.tableParams.reload();
        }, true);

        // Notifications
        $scope.notify = {
            errorStatus: function(error) {
                var msg = 'HTTP status code: ' + error.status;
                growl.message(msg, 'Error', 'error');
            },
            error: function(message) {
                growl.message(message, 'Error', 'error');
            },
            message: function(result, message) {
                var kind = result ? 'success' : 'error';
                var header = result ? 'Success' : 'Error';
                growl.message(message, header, kind);
            }
        };

        // Update on data:reload
        $scope.$on('data:reload', function() {
            $scope.users.read().then(undefined, $scope.notify.errorStatus);
        });

        // Table parameters
        $scope.tableParams = new ngTableParams({
            page: 1,
            count: 10
        }, {
            total: function() {
                return $scope.users.length;
            },
            getData: function($defer, params) {
                params.total($scope.users.length);
                var orderedData = params.sorting() ? $filter('orderBy')($scope.users, params.orderBy()) : $scope.users;
                $defer.resolve(orderedData.slice((params.page() - 1) * params.count(), params.page() * params.count()));
            },
            $scope: {
                $data: []
            }
        });

        // CRUD
        $scope.addUser = function() {
            $scope.openDialog(null);
        };

        $scope.updateUser = function(user) {
            $scope.openDialog(user);
        };

        $scope.removeUser = function(user) {
            var prompt = 'Withdraw user %key%@%realm% authorization?'
                .replace(/%key%/g, user.login || '???')
                .replace(/%realm%/g, user.realm || '???');

            dialogs.confirm('Withdraw authorization', prompt).result.then(function() {
                $scope.users.delete(user).then(function(reply) {
                    $scope.notify.message(reply.result, 'Withdraw authorization successful');
                }, $scope.notify.error);
            });
        };

        // Instance entry editor dialog
        $scope.openDialog = function(user) {
            var ModalInstanceCtrl = function($scope, $modalInstance, notify, users, realms, user) {
                $scope.isNew = !user;
                $scope.notify = notify;
                $scope.users = angular.isArray(users) ? users : [];
                $scope.realms = angular.isArray(realms) ? realms : [];
                $scope.user = angular.isObject(user) ? angular.copy(user) : { id: '', login: '', realm: '', admin_roles: [] };
                $scope.data = { selectedRealm: null };

                _.each($scope.realms, function(item) {
                    if (item.realm === $scope.user.realm)
                        $scope.data.selectedRealm = item;
                });

                if (!$scope.data.selectedRealm)
                    $scope.data.selectedRealm = $scope.realms[0];

                $scope.admin_roles = {
                    manage_users: $scope.isNew ? false : $scope.user.admin_roles.indexOf('manage_users') !== -1,
                    manage_instances: $scope.isNew ? false : $scope.user.admin_roles.indexOf('manage_instances') !== -1
                };

                $scope.closeModal = function(data) {
                    $modalInstance.close(data);
                };

                $scope.save = function() {
                    if ($scope.data.selectedRealm)
                        $scope.user.realm = $scope.data.selectedRealm.realm;

                    $scope.user.admin_roles = [];
                    $scope.admin_roles.manage_users && $scope.user.admin_roles.push('manage_users');
                    $scope.admin_roles.manage_instances && $scope.user.admin_roles.push('manage_instances');

                    var fnSave = $scope.isNew ? users.create : users.update;
                    fnSave($scope.user).then(function(reply) {
                        reply.result === false
                            ? $scope.notify.error(reply.error || 'Request failed')
                            : $scope.closeModal(reply);
                    }, $scope.notify.error);
                };

                $scope.cancel = function() {
                    $modalInstance.dismiss('cancel');
                };
            };

            var modalInstance = $modal.open({
                template: '<div template="user_edit.html"></div>',
                controller: ModalInstanceCtrl,
                resolve: {
                    notify: function() { return $scope.notify; },
                    users: function() { return $scope.users; },
                    realms: function() { return $scope.realms; },
                    user: function() { return user; }
                }
            });

            modalInstance.result.then(function(reply) {
                var op = user ? 'Update' : 'Add';
                var suffix = reply.result === false ? 'failed' : 'successful';

                $scope.notify.message(reply.result !== false, op + ' authorization ' + suffix);

                // Update current user
                if (reply.login === $scope.username && reply.realm === $scope. realm) {
                    $scope.roles.admin = utils.merge($scope.roles.admin, reply.admin_roles);
                }
            });
        };

        $scope.users.read();
        $scope.realms.read();
    }
]);
