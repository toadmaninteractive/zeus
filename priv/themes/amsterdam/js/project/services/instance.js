angular.module('zeusApp').service('instance', function($rootScope, $location, $q, api, storage, growl) {
    // Define menu item class
    function MenuItem(instance_id, item_info) {
        var instanceId = instance_id;
        var itemInfo = item_info;
        var pageInfo = {};
        var children = [];

        Object.defineProperty(this, 'itemInfo', {
            enumerable: true,
            get: function() {
                return itemInfo;
            }
        });

        Object.defineProperty(this, 'pageInfo', {
            enumerable: true,
            get: function() {
                return pageInfo;
            }
        });

        Object.defineProperty(this, 'items', {
            enumerable: true,
            get: function() {
                return children;
            }
        });

        this.update = function() {
            storage.getPageInfo(instanceId, itemInfo.route, function(reply) {
                pageInfo = reply;
            });
        };

        _.each(itemInfo.items, function(item) {
            children.push(new MenuItem(instanceId, item));
        });

        this.update();
    }

    // Define menu class
    function Menu(instance_id, menu_items) {
        var children = [];

        Object.defineProperty(this, 'items', {
            enumerable: true,
            get: function() {
                return children;
            }
        });

        _.each(menu_items, function(item) {
            children.push(new MenuItem(instance_id, item));
        });
    }

    // Define instance item class
    function Instance(server_info) {
        var instanceReady = false;
        var menuReady = false;
        var serverInfo = {};
        var agentInfo = {};
        var menu = [];

        Object.defineProperty(this, 'ready', {
            enumerable: true,
            get: function() {
                return instanceReady && menuReady;
            }
        });

        Object.defineProperty(this, 'serverInfo', {
            enumerable: true,
            get: function() {
                return serverInfo;
            }
        });

        Object.defineProperty(this, 'agentInfo', {
            enumerable: true,
            get: function() {
                return agentInfo;
            }
        });

        Object.defineProperty(this, 'menu', {
            enumerable: true,
            get: function() {
                return menu;
            }
        });

        this.update = function(server_info) {
            instanceReady = false;
            menuReady = false;
            serverInfo = server_info || {};

            api.remoteInstance(serverInfo.id, function(reply) {
                instanceReady = true;

                if (reply.result === false) {
                    if (api.loggedIn)
                        growl.message('Failed go get instance info', 'Error', 'error');
                } else {
                    agentInfo = reply;
                }
            });

            api.remoteMenu(serverInfo.id, function(reply) {
                menuReady = true;

                if (reply.result === false) {
                    if (api.loggedIn)
                        growl.message('Failed go get menu info', 'Error', 'error');
                } else {
                    menu = new Menu(serverInfo.id, reply.items);
                }
            });
        };

        this.update(server_info);
    }

    // Define instance collection class
    function InstanceCollection() {
        var items = [];
        var hashMap = {};
        var baseUrl = '/instances/entries';

        items.create = function(props) {
            // Create item
            var deferred = $q.defer();

            api.request('POST', baseUrl, {}, props, function(reply) {
                try {
                    var obj = new Instance(reply);
                    items.push(obj);
                    hashMap[obj.serverInfo.id] = obj;
                } catch (e) {
                } finally {
                    deferred.resolve(reply);
                }
            });

            return deferred.promise;
        };

        items.read = function(id) {
            var deferred = $q.defer();

            if (angular.isDefined(id) && hashMap.hasOwnProperty(id)) {
                // Read item
                api.request('GET', baseUrl + '/' + id, {}, {}, function(reply) {
                    try {
                        hashMap[id].update(reply);
                    } catch (e) {
                    } finally {
                        deferred.resolve(reply);
                    }
                });
            } else {
                // Read collection
                items.length = 0;
                hashMap = {};

                api.request('GET', baseUrl, {}, {}, function(reply) {
                    try {
                        _.each(reply, function(item) {
                            var obj = new Instance(item);
                            items.push(obj);
                            hashMap[item.id] = obj;
                        });
                    } catch (e) {
                    } finally {
                        deferred.resolve(reply);
                    }
                });
            }

            return deferred.promise;
        };

        items.update = function(id, props) {
            // Update item
            var deferred = $q.defer();

            if (angular.isDefined(id) && hashMap.hasOwnProperty(id)) {
                var serverInfo = hashMap[id].serverInfo;
                angular.extend(serverInfo, props);

                api.request('PUT', baseUrl + '/' + id, {}, serverInfo, function(reply) {
                    try {
                        !(reply.result === false) && hashMap[id].update(reply);
                    } catch (e) {
                    } finally {
                        deferred.resolve(reply);
                    }
                });
            }

            return deferred.promise;
        };

        items.delete = function(id) {
            // Delete item
            var deferred = $q.defer();

            if (angular.isDefined(id) && hashMap.hasOwnProperty(id)) {
                api.request('DELETE', baseUrl + '/' + id, {}, {}, function(reply) {
                    try {
                        if (reply.result) {
                            items.splice(items.indexOf(hashMap[id]), 1);
                            delete hashMap[id];
                        }
                    } catch (e) {
                    } finally {
                        deferred.resolve(reply);
                    }
                });
            }

            return deferred.promise;
        };

        items.clear = function() {
            items.length = 0;
        };

        Object.defineProperty(this, 'list', {
            enumerable: true,
            get: function() {
                return items;
            }
        });

        items.read();
    }

    var items = new InstanceCollection();

    $rootScope.$on('user:login', function(event, data) {
        items.list.read();
    });

    Object.defineProperty(this, 'items', {
        enumerable: true,
        get: function() {
            return items;
        }
    });

    this.helpers = {
        getKey: function(path) {
            var re = new RegExp('^\/([a-zA-Z0-9]{1}[a-zA-Z0-9_]+)(\/(.*))?$', []);
            var matches = path.match(re);

            return (matches && matches.length >= 2) ? matches[1] : undefined;
        },

        getId: function(path) {
            var key = this.getKey(path);

            var instance = _.find(items.list, function(item) {
                return item.serverInfo.key === key;
            });

            return instance ? instance.serverInfo.id : undefined;
        },

        getRoute: function(path) {
            var re = new RegExp('^\/[a-zA-Z0-9]{1}[a-zA-Z0-9_]*(\/(.*))?$', []);
            var matches = path.match(re);
            var route = (matches && matches.length >= 2) ? (matches[1] || '') : '';
            var parts = route.split('?');

            return parts[0] || '/';
        },

        itemById: function(id) {
            return _.find(items.list, function(item) {
                return item.serverInfo.id === id;
            });
        },

        itemByKey: function(key) {
            return _.find(items.list, function(item) {
                return item.serverInfo.key === key;
            });
        }
    };

    Object.defineProperty(this, 'active', {
        enumerable: true,
        get: function() {
            return {
                id: this.helpers.getId($location.path()),
                key: this.helpers.getKey($location.path()),
                route: this.helpers.getRoute($location.path()),
                item: this.helpers.itemById(this.helpers.getId($location.path()))
            };
        }
    });
});
