/**
 * ng-context-menu - v0.1.6 - An AngularJS directive to display a context menu when a right-click event is triggered
 *
 * @author Ian Kennington Walter (http://ianvonwalter.com)
 */
angular
    .module('ng-context-menu', [])
    .factory('ContextMenuService', function() {
        return {
            element: null,
            menuElement: null
        };
    })
    .directive('contextMenu', ['$document', '$timeout', 'ContextMenuService', 'utils', function($document, $timeout, ContextMenuService, utils) {
        return {
            restrict: 'A',
            scope: {
                'callback': '&contextMenu',
                'disabled': '&contextMenuDisabled'
            },
            link: function($scope, $element) {
                var opened = false;
                var lastAppearanceEvent = null;
                var menuPos, menuOffset;
                var deltaX = 10, deltaY = 10;

                function open(event, menuElement) {
                    if (!menuPos) {
                        menuPos = menuElement.position();
                        menuOffset = menuElement.offset();

                        if (menuPos !== undefined)
                            menuElement.css({ left: menuPos.left + 'px', top: menuPos.top + 'px' });
                    }

                    if (menuPos === undefined)
                        return;

                    var posX = menuPos.left + event.pageX - menuOffset.left + deltaX;
                    var posY = menuPos.top + event.pageY - menuOffset.top + deltaY;

                    menuElement.addClass('open').css({ left: posX  + 'px', top: posY + 'px', visibility: 'visible' });
                    opened = true;
                }

                function close(menuElement) {
                    menuElement.removeClass('open');
                    opened = false;
                }

                var contextMenuOptions = angular.fromJson($element.data('contextMenuOptions').replace(/'/g, '"'));

                if (contextMenuOptions !== '') {
                    if (contextMenuOptions.click !== 'null') {
                        if (contextMenuOptions.click.style === 'popover') {
                            $element.on('click', function(event) {
                                if (!$scope.disabled()) {
                                    if (contextMenuOptions.click.target === '')
                                        return;

                                    if (event.button !== 2) {
                                        $scope.$apply(function() {
                                            if (ContextMenuService.menuElement !== null)
                                                close(ContextMenuService.menuElement);
                                        });
                                    }

                                    bindPopover(event, contextMenuOptions.click.target);
                                }
                            });
                        } else {
                            $element.bind('click', function (event) {
                                opened && closeWindow(event);
                                bindPopup(event, contextMenuOptions.click.target);
                            });
                        }
                    }

                    if (contextMenuOptions.hover !== 'null') {
                        if (contextMenuOptions.hover.style === 'popover') {
                            $element.bind('mouseenter', function(event) {
                                bindPopover(event, contextMenuOptions.hover.target);
                            });
                        } else {
                            $element.bind('mouseenter', function (event) {
                                !opened && bindPopup(event, contextMenuOptions.hover.target);
                            });
                        }
                    }
                }

                function bindPopover(event, targetId) {
                    var popupElement = document.getElementById(targetId);

                    if (opened || popupElement === null)
                        return;

                    ContextMenuService.menuElement = angular.element(popupElement);
                    ContextMenuService.element = event.target;
                    ContextMenuService.element.id = utils.randomString();

                    event.preventDefault();
                    event.stopPropagation();

                    $scope.$apply(function () {
                        $scope.callback({ $event: event });
                        open(event, ContextMenuService.menuElement);
                    });

                    $element.bind('mouseleave', function(event) {
                        $scope.currentElement = ContextMenuService.menuElement;
                        $timeout(function() {
                            if (!ContextMenuService.menuElement.is(':hover')) {
                                closeWindow(event, $scope.currentElement);
                            }
                        }, 150);
                    });

                    ContextMenuService.menuElement.bind('mouseleave', function(event) {
                        if (angular.element(event.toElement).attr('id') !== ContextMenuService.element.id)
                            closeWindow(event);
                    });
                }

                function bindPopup(event, targetId) {
                    if ($scope.disabled())
                        return;

                    lastAppearanceEvent = event.type;

                    if (!opened) {
                        if (ContextMenuService.menuElement !== null)
                            close(ContextMenuService.menuElement);

                        if (targetId === undefined)
                            return;

                        var popupElement = document.getElementById(targetId);

                        if (popupElement === null)
                            return;

                        ContextMenuService.menuElement = angular.element(popupElement);
                        ContextMenuService.element = event.target;

                        event.preventDefault();
                        event.stopPropagation();

                        $scope.$apply(function () {
                            $scope.callback({ $event: event });
                            open(event, ContextMenuService.menuElement);
                        });
                    } else {
                        closeWindow(event);
                    }
                }

                function closeWindow(event, menuElement) {
                    if (event.button !== 2 || event.target !== ContextMenuService.element) {
                        $scope.$apply(function() {
                            close(menuElement === undefined ? ContextMenuService.menuElement : menuElement);
                        });
                    }
                }

                function handleKeyUpEvent(event) {
                    if (!$scope.disabled() && opened && event.keyCode === 27) {
                        $scope.$apply(function() {
                            close(ContextMenuService.menuElement);
                        });
                    }
                }

                function handleClickEvent(event) {
                    if (!$scope.disabled() && opened && event.target !== ContextMenuService.element) {
                        $scope.$apply(function() {
                            close(ContextMenuService.menuElement);
                        });
                    }
                }

                $document.bind('keyup', handleKeyUpEvent);

                // Firefox treats a right-click as a click and a contextmenu event while other browsers
                // just treat it as a contextmenu event
                $document.bind('click', handleClickEvent);
                $document.bind('contextmenu', handleClickEvent);

                $scope.$on('$destroy', function() {
                    $document.unbind('keyup', handleKeyUpEvent);
                    $document.unbind('click', handleClickEvent);
                    $document.unbind('contextmenu', handleClickEvent);
                });
            }
        };
    }]);
