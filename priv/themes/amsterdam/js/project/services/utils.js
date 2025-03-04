angular.module('zeusApp').service('utils', function($rootScope, $sce, $location) {
    this.broadcast = function(name, args) {
        $rootScope.$broadcast(name, args);
    };

    this.toStringSafe = function(value) {
        switch(value) {
            case undefined:
            case null:
                return '';
            default:
                return $sce.trustAsHtml(value.toString())
        }
    };

    this.randomString = function() {
        return Math.random().toString(36).substring(7);
    };

    this.unquote = function(str) {
        return str.replace(/^\'|\'$/g, '');
    };

    this.clear = function(a) {
        if (angular.isArray(a)) {
            a.length = 0;
            return [];
        } else if (angular.isObject(a)) {
            return {};
        } else {
            return undefined;
        }
    };

    this.clearWatchers = function(a) {
        if (angular.isArray(a) || angular.isObject(a)) {
            _.each(_.functions(a), function(fun) {
                angular.isFunction(a[fun]) && a[fun]();
            });
            return this.clear(a);
        } else {
            return this.clear(a);
        }
    };

    this.appendArray = function(array_to, array_from, reset) {
        var isArrayTo = angular.isArray(array_to);
        var isArrayFrom = angular.isArray(array_from);

        if (!isArrayTo && !isArrayFrom)
            return [];

        var arrTo = isArrayTo ? array_to : [];
        var arrFrom = isArrayFrom ? array_from : [];

        reset && (arrTo.length = 0);

        _.each(arrFrom, function(item) {
            arrTo.push(item);
        });

        return arrTo;
    };

    this.arrayToObject = function(array, key) {
        if (!angular.isArray(array) || !angular.isString(key) || !key.length)
            return {};

        var obj = {};

        _.each(array, function(item) {
            angular.isDefined(item[key]) && (obj[item[key]] = item);
        });

        return obj;
    };

    this.merge = function(a, b, preserve_keys) {
        var that = this;
        var preserve_all = preserve_keys === true;
        preserve_keys = angular.isArray(preserve_keys) ? preserve_keys : [];

        if (angular.isArray(a) && angular.isArray(b)) {
            a.length = a.length > b.length ? b.length : a.length;

            for (var i = 0; i < b.length; i++) {
                if (a.length <= i)
                    a.push(b[i]);
                else
                    a[i] = that.merge(a[i], b[i]);
            }
        } else if (angular.isObject(a) && angular.isObject(b)) {
            var keys = _.union(_.keys(a), _.keys(b));

            _.each(keys, function(key) {
                if (a.hasOwnProperty(key) && b.hasOwnProperty(key)) {
                    a[key] = that.merge(a[key], b[key], preserve_keys);
                } else if (a.hasOwnProperty(key) && !b.hasOwnProperty(key)) {
                    if (!preserve_all && preserve_keys.indexOf(key) === -1)
                        delete a[key];
                } else if (!a.hasOwnProperty(key) && b.hasOwnProperty(key)) {
                    a[key] = b[key];
                }
            });
        } else {
            a = b;
        }

        return a;
    };

    this.append = function(a, b) {
        var that = this;

        if (!angular.isObject(a) || !angular.isObject(b))
            return a;

        var keys = _.keys(b);

        _.each(keys, function(key) {
            a[key] = b[key];
        });

        return a;
    };

    this.json = function (str, fallback) {
        try {
            return JSON.parse(str);
        } catch (ex) {
            return fallback;
        }
    };

    this.makeWebsocketUrl = function(instance_id, path) {
        var isSecure = $location.protocol() === 'https';
        var wsProtocol = isSecure ? 'wss' : 'ws';
        var pathPrefix = (path.length > 0 && path[0] === '/') ? '' : '/';

        return wsProtocol + '://' + $location.host() + ':' + $location.port() + '/_ws/' + instance_id + pathPrefix + path;
    };
});
