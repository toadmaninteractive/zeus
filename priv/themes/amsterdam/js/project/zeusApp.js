var zeusApp = angular.module('zeusApp', [
    'ngRoute',
    'ngSanitize',
    // 'ngAnimate',
    'ui.bootstrap',
    'ngTable',
    'ui.chart',
    'ng-context-menu',
    'xeditable',
    'ui.select',
    'ngCookies',
    'LocalStorageModule',
    'sy.bootstrap.timepicker',
    'template/syTimepicker/timepicker.html',
    'template/syTimepicker/popup.html',
    'treeGrid',
    'ngWebSocket'
]);

zeusApp.config([
    '$provide', '$routeProvider', '$locationProvider', '$logProvider', '$sceProvider', 'localStorageServiceProvider',
    function ($provide, $routeProvider, $locationProvider, $logProvider, $sceProvider, localStorageServiceProvider) {
        // Turn off debug logging
        $logProvider.debugEnabled(true);

        // Decorate original $log service
        $provide.decorator('$log', function ($delegate) {
            // Original methods
            var origInfo = $delegate.info;
            var origLog = $delegate.log;
            var origError = $delegate.error;

            $delegate.info = function () {
                $logProvider.debugEnabled() && origInfo.apply(null, arguments);
            };

            $delegate.log = function () {
                $logProvider.debugEnabled() && origLog.apply(null, arguments);
            };

            $delegate.error = function () {
                $logProvider.debugEnabled() && origError.apply(null, arguments);
            };

            return $delegate;
        });

        // Enable HTML5 mode
        $locationProvider.html5Mode(true).hashPrefix('#!');

        // Enable smart routing
        $routeProvider
            .when('/:route*', { template: '', reloadOnSearch: false })
            .otherwise({ template: '', reloadOnSearch: false });

        // Turn on SCE
        $sceProvider.enabled(true);

        // Set prefix to local storage provider
        var hostCookie = window.location.host.replace(/[\.:-]/g, '_');
        localStorageServiceProvider.setPrefix('zeus_' + hostCookie);
    }
]);

zeusApp.run(function(reactive, instance, editableOptions, editableThemes) {
    // Set Bootstrap 3 theme for xEditable
    editableThemes.bs3.buttonsClass = 'btn-sm';
    editableOptions.theme = 'bs3';
});
