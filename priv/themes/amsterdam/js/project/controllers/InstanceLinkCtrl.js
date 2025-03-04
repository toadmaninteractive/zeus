angular.module('zeusApp').controller('InstanceLinkCtrl', [
    '$scope', '$document', '$location', '$window', '$q', '$timeout', '$log', 'instance', 'utils', 'storage', 'recent', 'api',
    function($scope, $document, $location, $window, $q, $timeout, $log, instance, utils, storage, recent, api) {
        $scope.$window = $window;
        $scope.instance = instance;
        $scope.utils = utils;
        $scope.recent = recent;
        $scope.JSON = JSON;

        // Linked instance page information
        $scope.pageInfo = {
            ready: false,
            update: function() {
                if (this.instance_id === instance.active.id && this.route === instance.active.route)
                    return;

                var that = this;
                this.ready = this.instance_id !== instance.active.id ? false : this.ready;
                this.instance_id = instance.active.id;
                this.route = instance.active.route;

                delete this.layout_url;
                delete this.data_url;

                if (!this.ready) {
                    var promise = $scope.loadConverters(instance.active.id);

                    var fnCallback = function() {
                        that.ready = true;
                    };

                    promise && promise.then(fnCallback, fnCallback);
                }

                storage.getPageInfo(instance.active.id, instance.active.route, function(reply) {
                    that.layout_url = reply.layout_url;
                    that.data_url = reply.data_url;
                    that.is_websocket = reply.is_websocket;
                    that.data = reply.data;
                });
            },
            isValid: function() {
                return this.hasOwnProperty('layout_url') && this.hasOwnProperty('instance_id') && this.ready === true;
            }
        };

        $scope.linkedContext = {
            convert: function(converter, value) {
                return angular.isObject($window.ZeusConverters) && angular.isFunction($window.ZeusConverters[converter])
                    ? $window.ZeusConverters[converter](value)
                    : value;
            },
            pageInfo: $scope.pageInfo,
            data: {
                // page: {},
                page: [],
                aux: {},
                qs: {}
            }
        };

        // Linked script
        $scope.linkedScript = {
            element: undefined,
            instance_id: undefined
        };

        // Dynamically load converters
        $scope.loadConverters = function(instance_id) {
            if (!instance_id || ($scope.linkedScript.instance_id === instance_id))
                return;

            $scope.linkedScript.deferred = $q.defer();
            $scope.linkedScript.instance_id = instance_id;

            try {
                $document[0].head.removeChild($scope.linkedScript.element);
            }
            catch (e)
            {
                $log.warn('Cannot remove converters element');
            }

            $scope.linkedScript.element = $document[0].createElement('script');

            $document[0].head.appendChild($scope.linkedScript.element);
            var el = $scope.linkedScript.element;

            el.onload = el.onreadystatechange = el.onerror = function(e) {
                $timeout(function () {
                    $scope.linkedScript.deferred.resolve(e);
                });
            };

            $scope.linkedScript.element.src = '_utils/api/instances/link/%instance_id%/zeus/converters.js?%random%'
                .replace(/%instance_id%/g, instance_id)
                .replace(/%random%/g, utils.randomString());

            return $scope.linkedScript.deferred.promise;
        };

        $scope.isPageVisible = function(kind) {
            var isAdminPage = instance.active.route === '/_admin';
            var isLogsPage = instance.active.route === '/_logs';

            switch (kind) {
                case 'instance': return !isAdminPage && !isLogsPage;
                case 'admin': return isAdminPage;
                case 'logs': return isLogsPage;
                default: return false;
            }
        };

        // Action logs
        $scope.actionLogs = {
            query: { date_from: moment().subtract(7, 'days').format('YYYY-MM-DD'), date_to: moment().format('YYYY-MM-DD') },
            result: null,
            error: '',
            formatItem: function(item) {
                return '\
                    <div class="mix-log-entry"> \
                        <span class="mix-log-date">[%datetime%]</span> \
                        <span class="mix-log-user">%username%@%realm%</span> \
                        <span class="mix-log-message">%message%</span> \
                    </div>'
                    .replace(/%datetime%/g, item.datetime)
                    .replace(/%username%/g, item.username)
                    .replace(/%realm%/g, item.realm)
                    .replace(/%message%/g, item.message);
            },
            formatResult: function() {
                if (this.error)
                    return this.error;
                else if (!angular.isArray(this.result))
                    return 'No data';
                else if (this.result.length < 1)
                    return 'No results';

                return _.map(this.result, this.formatItem).join('');
            },
            validateQuery: function() {
                var dateFrom = moment($scope.actionLogs.query.date_from);
                var dateTo = moment($scope.actionLogs.query.date_to);

                if (dateTo.isBefore(dateFrom)) {
                    $scope.actionLogs.query.date_from = dateTo.format('YYYY-MM-DD');
                    $scope.actionLogs.query.date_to = dateFrom.format('YYYY-MM-DD');
                }
            },
            request: function() {
                this.validateQuery();

                var that = this;
                var qsVals = angular.copy($location.search());
                $scope.actionLogs.query.date_from && (qsVals.date_from = moment($scope.actionLogs.query.date_from).format('YYYY-MM-DD'));
                $scope.actionLogs.query.date_to && (qsVals.date_to = moment($scope.actionLogs.query.date_to).format('YYYY-MM-DD'));

                api.request('GET', '/instance/logs/' + instance.active.id, qsVals, {}, function(reply) {
                    if (angular.isArray(reply)) {
                        that.error = '';
                        that.result = angular.isArray(that.result) ? utils.appendArray(that.result, reply, true) : reply;
                    } else {
                        angular.isArray(that.result) && (that.result.length = 0);
                        that.error = reply.error ? reply.error : 'Server error';
                    }
                });
            }
        };

        // Update page info on active instance update
        $scope.$watch(function() {
            return instance.active;
        }, function() {
            $scope.pageInfo.update();
            $scope.actionLogs.result = null;

            if ($scope.isPageVisible('logs') && _.keys($location.search()).length > 0)
                $scope.actionLogs.request();
        }, true);

        $scope.$on('data:reload', function() {
            if ($scope.isPageVisible('logs'))
                $scope.actionLogs.request();
        });

        // Handle page:debug event
        $scope.$on('page:debug', function() {
            $log.log('Linked instance context:');
            $log.log($scope.linkedContext);
            $log.log('------------------------');
        });

        $scope.$on('$destroy', function() {
            try {
                $document[0].body.removeChild($scope.linkedScript.element);
            }
            catch (e)
            {
                $log.warn('Cannot remove converters element: none present');
            }
        });

        $scope.$watch(function() {
            return $location.search();
        }, function(new_val) {
            utils.merge($scope.linkedContext.data.qs, new_val);
        }, true);

        // Initial page information update
        $scope.pageInfo.update();
    }
]);
