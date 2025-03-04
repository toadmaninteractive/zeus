angular.module('zeusApp').controller('RealmRolesViewCtrl', [
    '$scope', '$filter', '$modal', '$q', 'ngTableParams', 'api', 'instance', 'dialogs', 'growl', 'utils',
    function($scope, $filter, $modal, $q, ngTableParams, api, instance, dialogs, growl, utils) {
        // Store services
        $scope.utils = utils;

        // Data
        $scope.baseUrl = '/auth/realm-roles';
        $scope.realms = [];
        $scope.realmMap = {};
        $scope.realmRoles = [];
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
        $scope.realmRoles.create = function(realmRole) {
            var deferred = $q.defer(),
                url = [$scope.baseUrl, instance.active.id].join('/');

            api.request('POST', url, {}, realmRole, function(reply) {
                if (reply.result !== false)
                    $scope.realmRoles.push(realmRole);

                deferred.resolve(reply);
            });

            return deferred.promise;
        };

        $scope.realmRoles.read = function() {
            var deferred = $q.defer(),
                url = [$scope.baseUrl, instance.active.id].join('/');

            api.request('GET', url, {}, {}, function(reply) {
                if (angular.isArray(reply))
                    $scope.realmRoles = utils.merge($scope.realmRoles, reply);

                deferred.resolve(reply);
            });

            return deferred.promise;
        };

        $scope.realmRoles.update = function(realmRole) {
            var deferred = $q.defer(),
                url = [$scope.baseUrl, instance.active.id, realmRole.name].join('/');

            api.request('PUT', url, {}, realmRole, function(reply) {
                if (reply.result !== false) {
                    var found = _.findWhere($scope.realmRoles, { name: realmRole.name, instance_id: realmRole.instance_id });
                    utils.merge(found, realmRole);
                }

                deferred.resolve(reply);
            });

            return deferred.promise;
        };

        $scope.realmRoles.delete = function(realmRole) {
            var deferred = $q.defer(),
                url = [$scope.baseUrl, instance.active.id, realmRole.name].join('/');

            api.request('DELETE', url, {}, {}, function(reply) {
                if (reply.result !== false) {
                    var found = _.findWhere($scope.realmRoles, { name: realmRole.name, instance_id: realmRole.instance_id });

                    if (found)
                        $scope.realmRoles.splice($scope.realmRoles.indexOf(found), 1);
                }

                deferred.resolve(reply);
            });

            return deferred.promise;
        };

        // Watch user entries
        $scope.$watch(function() {
            return $scope.realmRoles;
        }, function() {
            $scope.tableParams.reload();
        }, true);

        // Watch active instance ID
        $scope.$watch(function() {
            return instance.active.id;
        }, function(new_id, old_id) {
            if (new_id !== old_id && +new_id > 0) {
                if ($scope.canFetchRoles(+new_id)) {
                    $scope.realmRoles.read().then(undefined, $scope.notify.errorStatus);
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
            // $scope.realmRoles.read().then(undefined, $scope.notify.errorStatus);
        });

        // Table parameters
        $scope.tableParams = new ngTableParams({
            page: 1,
            count: 10
        }, {
            total: function() {
                return $scope.realmRoles.length;
            },
            getData: function($defer, params) {
                params.total($scope.realmRoles.length);
                var orderedData = params.sorting() ? $filter('orderBy')($scope.realmRoles, params.orderBy()) : $scope.realmRoles;
                $defer.resolve(orderedData.slice((params.page() - 1) * params.count(), params.page() * params.count()));
            },
            $scope: {
                $data: []
            }
        });

        // CRUD
        $scope.addRealmRoles = function() {
            $scope.openDialog(null);
        };

        $scope.updateRealmRoles = function(realmRole) {
            $scope.openDialog(realmRole);
        };

        $scope.removeRealmRoles = function(realmRole) {
            var prompt = 'Withdraw instance roles for realm %realm%?'.replace(/%realm%/g, realmRole.name || '???');

            dialogs.confirm('Withdraw authorization', prompt).result.then(function() {
                $scope.realmRoles.delete(realmRole).then(function(reply) {
                    $scope.notify.message(reply.result, 'Withdraw instance roles successful');
                }, $scope.notify.error);
            });
        };

        // Instance roles entry editor dialog
        $scope.openDialog = function(realmRole) {
            var ModalInstanceCtrl = function($scope, $modalInstance, notify, realmRoles, realms, realmRole) {
                $scope.isNew = !realmRole;
                $scope.notify = notify;
                $scope.realmRoles = angular.isArray(realmRoles) ? realmRoles : [];
                $scope.realms = angular.isArray(realms) ? realms : [];
                $scope.realmRole = angular.isObject(realmRole) ? angular.copy(realmRole) : { id: '', name: '', roles: [] };
                $scope.data = { selectedRealm: null };
                $scope.instanceRoles = angular.isObject(realmRole) ? angular.copy(realmRole.roles) : [];
                $scope.agentRoles = instance.active.item.agentInfo.roles;

                _.each($scope.realms, function(item) {
                    if (item.realm === $scope.realmRole.name)
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
                        $scope.realmRole.name = $scope.data.selectedRealm.realm;

                    $scope.realmRole.roles = [];

                    _.each($scope.agentRoles, function(role) {
                        if ($scope.roleFlags[role.role] === true)
                            $scope.realmRole.roles.push(role.role);
                    });

                    var fnSave = $scope.isNew ? realmRoles.create : realmRoles.update;

                    fnSave($scope.realmRole).then(function(reply) {
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
                template: '<div template="realm_roles_edit.html"></div>',
                controller: ModalInstanceCtrl,
                resolve: {
                    notify: function() { return $scope.notify; },
                    realmRoles: function() { return $scope.realmRoles; },
                    realms: function() { return $scope.realms; },
                    realmRole: function() { return realmRole; }
                }
            });

            modalInstance.result.then(function(reply) {
                var op = realmRole ? 'Update' : 'Add';
                var suffix = reply.result === false ? 'failed' : 'successful';
                $scope.notify.message(reply.result !== false, op + ' instance roles ' + suffix);
            });
        };

        if ($scope.isAdmin || (angular.isArray($scope.roles.user[instance.active.id]) && $scope.roles.user[instance.active.id].indexOf('admin') !== -1)) {
            $scope.realmRoles.read();
            $scope.realms.read();
        }
    }
]);
