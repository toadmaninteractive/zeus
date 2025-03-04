angular.module('zeusApp').service('api', function($http, $log, growl, utils) {
    var httpStatusCodes = {
        400: 'Bad Request',
        401: 'Unauthorized',
        402: 'Payment Required',
        403: 'Forbidden',
        404: 'Not Found',
        405: 'Method Not Allowed',
        406: 'Not Acceptable',
        407: 'Proxy Authentication Required',
        408: 'Request Timeout',
        409: 'Conflict',
        410: 'Gone',
        411: 'Length Required',
        412: 'Precondition Failed',
        413: 'Request Entity Too Large',
        414: 'Request-URI Too Long',
        415: 'Unsupported Media Type',
        416: 'Requested Range Not Satisfiable',
        417: 'Expectation Failed'
    };

    var loggedIn = false;

    Object.defineProperty(this, 'loggedIn', {
        enumerable: true,
        get: function() {
            return loggedIn;
        }
    });

    this.requestUrl = function(http_method, url, qs_vals, body, callback) {
        var http_methods = ['GET', 'HEAD', 'PUT', 'POST', 'PATCH', 'DELETE', 'OPTIONS'];
        http_method = _.contains(http_methods, http_method) ? http_method : http_methods[0];
        var request_failed = {result: false, error: 'request_failed'};
        var qs = _.map(qs_vals, function(value, key) { return encodeURIComponent(key) + '=' + encodeURIComponent(value); }).join('&');

        if (qs)
            qs = (url.indexOf('?') !== -1 ? '&' : '?') + qs;

        if (angular.isString(body))
            body = '"' + body + '"';
        else if (typeof body === 'boolean')
            body = body.toString();

        $http({ url: url + qs, method: http_method, data: body })
            .success(function(reply_data, status, headers, config) {
                var res = angular.isString(reply_data) ? 'HTML' : reply_data;
                $log.info('--> ', http_method, url + qs, ' OK, body: ', body, ', result: ', res);

                if (angular.isFunction(callback))
                    callback(reply_data);

                if (reply_data.result === false && reply_data.error === 'not_authenticated')
                    utils.broadcast('api:not_authenticated');
            })
            .error(function(reply_data, status, headers, config) {
                var res = angular.isString(reply_data) ? 'HTML' : reply_data;
                $log.error('--> ', http_method, url + qs, ' Error, body: ', body, ', result: ', res);

                if (loggedIn) {
                    var statusMessage = httpStatusCodes[status] ? httpStatusCodes[status] : '';
                    growl.message(http_method + ' ' + url, 'Request failed: ' + status + ' ' + statusMessage, 'error', 5000);
                }

                if (angular.isFunction(callback))
                    callback(request_failed);
            });
    };

    this.request = function(http_method, url, qs_vals, body, callback) {
        var fullUrl = ['', '_utils', 'api', url].join('/').replace(/[\/]+/g, '/');
        this.requestUrl(http_method, fullUrl, qs_vals, body, callback);
    };

    this.requestRemoteApi = function(http_method, instance_id, url, qs_vals, body, callback) {
        var fullUrl = ['', '_utils', 'api', 'instances', 'link', instance_id, url].join('/').replace(/[\/]+/g, '/');
        this.requestUrl(http_method, fullUrl, qs_vals, body, callback);
    };

    this.login = function(username, password, realm, callback) {
        var body = { username: username, password: password, realm: realm };

        this.request('POST', '/auth/login', {}, body, function(reply) {
            loggedIn = angular.isObject(reply) && reply.result === true;

            if (angular.isFunction(callback))
                callback(reply);
        });
    };

    this.logout = function(callback) {
        this.request('GET', '/auth/logout', {}, {}, function(reply) {
            loggedIn = false;

            if (angular.isFunction(callback))
                callback(reply);
        });
    };

    this.isLoggedIn = function() {
        return loggedIn;
    };

    this.state = function(callback) {
        this.request('GET', '/auth/state', {}, {}, function(reply) {
            loggedIn = angular.isObject(reply) && reply.result === true;

            if (angular.isFunction(callback))
                callback(reply);
        });
    };

    this.realms = function(callback) {
        this.request('GET', '/auth/realms', {}, {}, callback);
    };

    this.version = function(callback) {
        this.request('GET', '/auth/version', {}, {}, callback);
    };

    this.localTemplate = function(template_name, callback) {
        this.requestUrl('GET', '/templates/' + template_name, {}, {}, callback);
    };

    this.remoteInstance = function(instance_id, callback) {
        this.requestRemoteApi('GET', instance_id, '/zeus/instance', {}, {}, callback);
    };

    this.remoteMenu = function(instance_id, callback) {
        this.requestRemoteApi('GET', instance_id, '/zeus/menu', {}, {}, callback);
    };

    this.remotePage = function(instance_id, page_url, callback) {
        var url = ('/zeus/pages/' + page_url).replace(/[\/]+/g, '/');
        this.requestRemoteApi('GET', instance_id, url, {}, {}, callback);
    };

    this.remoteLayout = function(instance_id, layout_url, callback) {
        var url = ('/zeus/layouts/' + layout_url).replace(/[\/]+/g, '/');
        this.requestRemoteApi('GET', instance_id, url, {}, {}, callback);
    };

    this.remoteSearch = function(instance_id, input, callback) {
        var url = ('/zeus/search').replace(/[\/]+/g, '/');
        this.requestRemoteApi('GET', instance_id, url, { input: input }, {}, callback);
    };

    this.getInstanceUrl = function(instance_id, url) {
        return ['', '_utils', 'api', 'instances', 'link', instance_id, url].join('/').replace(/[\/]+/g, '/');
    };
});
