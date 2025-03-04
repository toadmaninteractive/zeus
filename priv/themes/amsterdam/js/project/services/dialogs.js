angular.module('zeusApp').service('dialogs', function($modal) {
    this.confirm = function(caption, prompt) {
        var template = '\
            <div class="modal-header"> \
                <h5>{{ data.caption }}</h5> \
            </div> \
            <div class="modal-body"> \
                <h4><small>{{ data.prompt }}</small></h4> \
            </div> \
            <div class="modal-footer"> \
                <button class="btn btn-primary" ng-click="yes();">Yes</button> \
                <button class="btn" ng-click="no();">No</button> \
            </div>';

        var ModalInstanceCtrl = function($scope, $modalInstance, data) {
            $scope.data = data;
            $scope.yes = function() {
                $modalInstance.close(true);
            };
            $scope.no = function() {
                $modalInstance.dismiss('cancel');
            };
        };

        return $modal.open({
            template: template,
            controller: ModalInstanceCtrl,
            resolve: {
                data: function () {
                    return {
                        caption: caption,
                        prompt: prompt
                    };
                }
            }
        });
    };
});
