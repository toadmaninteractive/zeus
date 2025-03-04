angular.module('zeusApp').controller('InstanceRolesViewCtrl', [
    '$scope', '$filter', '$modal', '$q', 'ngTableParams', 'api', 'instance', 'dialogs', 'growl', 'utils',
    function($scope, $filter, $modal, $q, ngTableParams, api, instance, dialogs, growl, utils) {
        // Store services
        $scope.utils = utils;

        // Data
        $scope.baseUrl = '/auth/roles';
        $scope.realms = [];
        $scope.realmMap = {};
        $scope.userRoles = [];
        $scope.isAdmin = $scope.is_root || $scope.roles.admin.indexOf('manage_instances') !== -1;

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

        // Attach user roles functions
        $scope.userRoles.create = function(userRole) {
            var deferred = $q.defer(),
                url = [$scope.baseUrl, instance.active.id].join('/');

            api.request('POST', url, {}, userRole, function(reply) {
                if (reply.result !== false)
                    $scope.userRoles.push(userRole);

                deferred.resolve(reply);
            });

            return deferred.promise;
        };

        $scope.userRoles.read = function() {
            var deferred = $q.defer(),
                url = [$scope.baseUrl, instance.active.id].join('/');

            api.request('GET', url, {}, {}, function(reply) {
                if (angular.isArray(reply))
                    $scope.userRoles = utils.merge($scope.userRoles, reply);

                deferred.resolve(reply);
            });

            return deferred.promise;
        };

        $scope.userRoles.update = function(userRole) {
            var deferred = $q.defer(),
                url = [$scope.baseUrl, instance.active.id, userRole.realm, userRole.login].join('/');

            api.request('PUT', url, {}, userRole, function(reply) {
                if (reply.result !== false) {
                    var found = _.findWhere($scope.userRoles, { login: userRole.login, realm: userRole.realm });
                    utils.merge(found, userRole);
                }

                deferred.resolve(reply);
            });

            return deferred.promise;
        };

        $scope.userRoles.delete = function(userRole) {
            var deferred = $q.defer(),
                url = [$scope.baseUrl, instance.active.id, userRole.realm, userRole.login].join('/');

            api.request('DELETE', url, {}, {}, function(reply) {
                if (reply.result !== false) {
                    var found = _.findWhere($scope.userRoles, { login: userRole.login, realm: userRole.realm });

                    if (found)
                        $scope.userRoles.splice($scope.userRoles.indexOf(found), 1);
                }

                deferred.resolve(reply);
            });

            return deferred.promise;
        };

        // Watch user entries
        $scope.$watch(function() {
            return $scope.userRoles;
        }, function() {
            $scope.tableParams.reload();
        }, true);

        // Watch active instance ID
        $scope.$watch(function() {
            return instance.active.id;
        }, function(new_id, old_id) {
            if (new_id !== old_id && +new_id > 0) {
                if ($scope.canFetchRoles(+new_id)) {
                    $scope.userRoles.read().then(undefined, $scope.notify.errorStatus);
                }
            }
        }, true);

        $scope.canFetchRoles = function(instanceId) {
            var isExplicitInstanceAdmin = angular.isArray($scope.roles.user[instanceId])
                ? ($scope.roles.user[instanceId].indexOf('admin') !== -1)
                : false;

            return $scope.isAdmin || isExplicitInstanceAdmin;
        };

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
            // FIXME: improper event triggering (always on data:reload)
            // $scope.userRoles.read().then(undefined, $scope.notify.errorStatus);
        });

        // Table parameters
        $scope.tableParams = new ngTableParams({
            page: 1,
            count: 10
        }, {
            total: function() {
                return $scope.userRoles.length;
            },
            getData: function($defer, params) {
                params.total($scope.userRoles.length);
                var orderedData = params.sorting() ? $filter('orderBy')($scope.userRoles, params.orderBy()) : $scope.userRoles;
                $defer.resolve(orderedData.slice((params.page() - 1) * params.count(), params.page() * params.count()));
            },
            $scope: {
                $data: []
            }
        });

        // CRUD
        $scope.addInstanceRoles = function() {
            $scope.openDialog(null);
        };

        $scope.updateInstanceRoles = function(userRole) {
            $scope.openDialog(userRole);
        };

        $scope.removeInstanceRoles = function(userRole) {
            var prompt = 'Withdraw instance roles for %key%@%realm%?'
                .replace(/%key%/g, userRole.login || '???')
                .replace(/%realm%/g, userRole.realm || '???');

            dialogs.confirm('Withdraw authorization', prompt).result.then(function() {
                $scope.userRoles.delete(userRole).then(function(reply) {
                    $scope.notify.message(reply.result, 'Withdraw instance roles successful');
                }, $scope.notify.error);
            });
        };

        // Instance roles entry editor dialog
        $scope.openDialog = function(userRole) {
            var ModalInstanceCtrl = function($scope, $modalInstance, notify, userRoles, realms, userRole) {
                $scope.isNew = !userRole;
                $scope.notify = notify;
                $scope.userRoles = angular.isArray(userRoles) ? userRoles : [];
                $scope.realms = angular.isArray(realms) ? realms : [];
                $scope.userRole = angular.isObject(userRole) ? angular.copy(userRole) : { id: '', login: '', realm: '', roles: [] };
                $scope.data = { selectedRealm: null };
                $scope.instanceRoles = angular.isObject(userRole) ? angular.copy(userRole.roles) : [];
                $scope.agentRoles = instance.active.item.agentInfo.roles;

                _.each($scope.realms, function(item) {
                    if (item.realm === $scope.userRole.realm)
                        $scope.data.selectedRealm = item;
                });

                if (!$scope.data.selectedRealm)
                    $scope.data.selectedRealm = $scope.realms[0];

                $scope.roleFlags = {};

                _.each($scope.agentRoles, function(role) {
                    $scope.roleFlags[role.role] = $scope.instanceRoles.indexOf(role.role) !== -1;
                });

                $scope.closeModal = function(data) {
                    $modalInstance.close(data);
                };

                $scope.save = function() {
                    if ($scope.data.selectedRealm)
                        $scope.userRole.realm = $scope.data.selectedRealm.realm;

                    $scope.userRole.roles = [];

                    _.each($scope.agentRoles, function(role) {
                        if ($scope.roleFlags[role.role] === true)
                            $scope.userRole.roles.push(role.role);
                    });

                    var fnSave = $scope.isNew ? userRoles.create : userRoles.update;

                    fnSave($scope.userRole).then(function(reply) {
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
                template: '<div template="instance_roles_edit.html"></div>',
                controller: ModalInstanceCtrl,
                resolve: {
                    notify: function() { return $scope.notify; },
                    userRoles: function() { return $scope.userRoles; },
                    realms: function() { return $scope.realms; },
                    userRole: function() { return userRole; }
                }
            });

            modalInstance.result.then(function(reply) {
                var op = userRole ? 'Update' : 'Add';
                var suffix = reply.result === false ? 'failed' : 'successful';

                $scope.notify.message(reply.result !== false, op + ' instance roles ' + suffix);

                // Update current user roles
                if (reply.login === $scope.username && reply.realm === $scope.realm) {
                    var batch = {};
                    batch[instance.active.id] = reply.roles;
                    $scope.roles.user = utils.merge($scope.roles.user, batch);
                }
            });
        };

        if ($scope.isAdmin || (angular.isArray($scope.roles.user[instance.active.id]) && $scope.roles.user[instance.active.id].indexOf('admin') !== -1)) {
            $scope.userRoles.read();
            $scope.realms.read();
        }
    }
]);
