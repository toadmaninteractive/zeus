angular.module('zeusApp').constant('NgTables', {
    'ng-table/pager.html': '\
        <div class="ng-cloak ng-table-pager table-footer clearfix"> \
            <div ng-if="params.settings().counts.length" class="pagination ng-table-counts pull-right table-actions"> \
                <ul> \
                    <li class="caption"><span>Items per page:</span></li> \
                </ul> \
                <ul class="ng-table-pagination"> \
                    <li ng-class="{\'active\': params.count() === count}" ng-repeat="count in params.settings().counts"> \
                        <a ng-class="{\'current\': params.count() === count}" ng-click="params.count(count)" href=""><span ng-bind="count"></span></a> \
                    </li> \
                </ul> \
            </div> \
            <div class="pagination"> \
                <ul class="ng-table-pagination"> \
                    <li ng-class="{\'active\': page.number === params.page()}" ng-repeat="page in pages" ng-switch="page.type"> \
                        <a ng-switch-when="prev" ng-click="params.page(page.number)" href="">&laquo;</a> \
                        <a ng-switch-when="first" ng-click="params.page(page.number)" ng-class="{\'current\': page.number === params.page()}" href=""><span ng-bind="page.number"></span></a> \
                        <a ng-switch-when="page" ng-click="params.page(page.number)" ng-class="{\'current\': page.number === params.page()}" href=""><span ng-bind="page.number"></span></a> \
                        <a ng-switch-when="more" ng-click="params.page(page.number)" href="">&#8230;</a> \
                        <a ng-switch-when="last" ng-click="params.page(page.number)" ng-class="{\'current\': page.number === params.page()}" href=""><span ng-bind="page.number"></span></a> \
                        <a ng-switch-when="next" ng-click="params.page(page.number)" href="">&raquo;</a> \
                    </li> \
                </ul> \
            </div> \
        </div>'
});
