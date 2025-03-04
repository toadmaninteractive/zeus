angular.module('zeusApp').filter('unsafe', function($sce) {
    return $sce.trustAsHtml;
});
