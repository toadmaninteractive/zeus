angular.module('zeusApp').controller('InstancesViewCtrl', [
    '$scope', '$filter', '$modal', 'ngTableParams', 'instance', 'dialogs', 'growl', 'utils',
    function($scope, $filter, $modal, ngTableParams, instance, dialogs, growl, utils) {
        // Store services
        $scope.utils = utils;

        // Watch instance entries
        $scope.$watch(function() {
            return instance.items.list;
        }, function() {
            $scope.tableParams.reload();
        }, true);

        // Notifications
        $scope.notify = {
            error: function(error) {
                var msg = 'HTTP status code: ' + error.status;
                growl.message(msg, 'Error', 'error');
            },
            message: function(result, message) {
                var kind = result ? 'success' : 'error';
                var header = result ? 'Success' : 'Error';
                growl.message(message, header, kind);
            }
        };

        // Update on data:reload
        $scope.$on('data:reload', function() {
            instance.items.list.read().then(undefined, $scope.notify.error);
        });

        // Table parameters
        $scope.tableParams = new ngTableParams({
            page: 1,
            count: 10
        }, {
            total: function() {
                return instance.items.list.length;
            },
            getData: function($defer, params) {
                params.total(instance.items.list.length);
                var orderedData = params.sorting() ? $filter('orderBy')(instance.items.list, params.orderBy()) : instance.items.list;
                $defer.resolve(orderedData.slice((params.page() - 1) * params.count(), params.page() * params.count()));
            },
            $scope: {
                $data: []
            }
        });

        // CRUD
        $scope.createInstance = function() {
            $scope.openDialog(null);
        };

        $scope.updateInstance = function(item) {
            $scope.openDialog(item);
        };

        $scope.deleteInstance = function(item) {
            var prompt = "Delete instance %key% permanently?".replace(/%key%/g, item.key || "???");

            dialogs.confirm("Delete instance", prompt).result.then(function() {
                instance.items.list.delete(item.id).then(function(reply) {
                    $scope.notify.message(reply.result, 'Delete operation successful');
                }, $scope.notify.error);
            });
        };

        // Instance entry editor dialog
        $scope.openDialog = function(item) {
            var ModalInstanceCtrl = function($scope, $modalInstance, instance, notify, item) {
                $scope.isNew = !item;
                $scope.notify = notify;
                $scope.item = angular.isObject(item) ? angular.copy(item) : { id: '', key: '', title: '', api_root: '', api_key: '' };

                $scope.closeModal = function(data) {
                    $modalInstance.close(data);
                };

                $scope.save = function() {
                    $scope.isNew
                        ? instance.items.list.create($scope.item).then($scope.closeModal, $scope.notify.error)
                        : instance.items.list.update($scope.item.id, $scope.item).then($scope.closeModal, $scope.notify.error);
                };

                $scope.cancel = function() {
                    $modalInstance.dismiss('cancel');
                };
            };

            var modalInstance = $modal.open({
                template: '<div template="instance_edit.html"></div>',
                controller: ModalInstanceCtrl,
                resolve: {
                    instance: function() { return instance; },
                    notify: function() { return $scope.notify; },
                    item: function() { return item; }
                }
            });

            modalInstance.result.then(function(reply) {
                var op = item ? 'Update' : 'Create';
                var suffix = reply.result === false ? 'failed' : 'successful';

                $scope.notify.message(reply.result !== false, op + ' operation ' + suffix);
            });
        };
    }
]);
