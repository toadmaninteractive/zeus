angular.module('zeusApp').config(function($provide) {
    $provide.decorator('reactive', function($delegate, $compile, $parse, $interpolate, $timeout, api, instance, utils) {
        $delegate.addRenderer('table', function(parent_container, element, context_path, context) {
            // Update parent container style
            parent_container.style.display = 'block';
            var parentElement = angular.element(parent_container);

            // Get bindings
            var bindings = $delegate.getBindings(element, context_path);

            // Add compact class
            if (bindings.compact.value)
                parentElement.addClass('compact');

            // Define aux data container and its path
            var auxId = bindings.url.value ? $delegate.generateAuxId() : '';
            $delegate.context.data.aux[auxId] = [];
            var contextPathTable = bindings.url.value ? [$delegate.contextPath, 'data.aux', auxId].join('.') : bindings.rows.value;
            var contextTable = bindings.url.value ? $delegate.context.data.aux[auxId] : $parse(contextPathTable)($delegate.context.scope);

            // HACK: if model is undefined ATM
            if (!bindings.url.value && contextTable === undefined) {
                var model = $parse(contextPathTable);
                var value = model($delegate.context.scope);
                !value && model.assign($delegate.context.scope, []);
                contextTable = model($delegate.context.scope);
            }

            // Add table
            var oTable;
            var templateTable = '<table class="table"><thead><tr></tr></thead></table>';
            var elemTable = angular.element(templateTable);
            parent_container.appendChild(elemTable[0]);

            // Cache expressions
            var cacheContext = {};

            // Define table options
            var tableOptions = {
                bJQueryUI: false,
                bProcessing: true,
                sDom: '<"datatable-header"l><"datatable-scroll"t><"datatable-footer"ip>',
                order: [],
                paging: bindings.pagination.value,
                sPaginationType: 'full_numbers',
                iDisplayLength: 25,
                isVirtual: !!bindings.url.value,
                virtualLength: 25,
                virtualStart: 0,
                virtualTotalCount: 0,
                columns: _.map(element.columns, function(column) {
                    // Get substitution values
                    var substituteColumnSortBy = $delegate.isBinding(column.sort_by) ? column.sort_by.binding.join('') : undefined;

                    // Check alignment
                    var alignClass = 'text-left';

                    if (column.align) {
                        var alignValue = $delegate.isBinding(column.align)
                            ? $parse(column.align.value)($delegate.context.scope)
                            : $interpolate(column.align.content)($delegate.context.scope);

                        switch (alignValue) {
                            case 'left': alignClass = 'text-left'; break;
                            case 'center': alignClass = 'text-center'; break;
                            case 'right': alignClass = 'text-right'; break;
                            default: alignClass = 'text-left'; break;
                        }
                    }

                    return {
                        data: angular.identity,
                        bSortable: !!substituteColumnSortBy,
                        sortBy: substituteColumnSortBy || undefined,
                        sType: 'zeus',
                        className: alignClass,
                        orderDataType: 'zeus',
                        render: function(data, type, row, meta) {
                            // Get substitution values
                            var substituteColumnSortBy = $delegate.isBinding(column.sort_by) ? column.sort_by.binding.join('') : undefined;
                            var value = substituteColumnSortBy ? (data[substituteColumnSortBy] || '') : '';

                            return '<span class="sort-by" style="display: none;">%value%</span>'
                                .replace(/%value%/g, value);
                        }
                    };
                }),
                fnRowCallback: function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {
                    var elemRow = angular.element(nRow);
                    var realIndex = contextTable.indexOf(aData);

                    var contextRowPath = [contextPathTable, '[', realIndex.toString(), ']'].join('');

                    if (elemRow.hasClass('x-compiled'))
                        return;

                    // Render cells
                    for(var index = 0; index < nRow.childNodes.length; index++) {
                        // Acquire cell node and appropriate column
                        var column = element.columns[index];
                        var cell = nRow.childNodes[index];
                        var elemCell = angular.element(cell);

                        // Get bindings
                        var bindingContent = $delegate.getBinding(column.content, contextRowPath);

                        // Empty cell
                        //elemCell.empty();

                        // Get cell context
                        var cellContextPath = bindingContent.kind === 'binding' ? bindingContent.value : contextRowPath;
                        !cacheContext.hasOwnProperty(cellContextPath) && (cacheContext[cellContextPath] = $parse(cellContextPath));
                        var cellContext = cacheContext[cellContextPath];

                        // Render content
                        if (column.content_template) {
                            // Render template
                            $delegate.render(cell, column.content_template, cellContextPath, cellContext);
                        } else {
                            // Substitute text
                            var substituteContentText = bindingContent.kind === 'binding'
                                ? ['utils.toStringSafe(', bindingContent.value, ')'].join('')
                                : ["\'", bindingContent.value + '', "\'"].join('');

                            cell.setAttribute('ng-bind-html', substituteContentText);
                        }
                    }

                    $compile(elemRow.addClass('x-compiled'))($delegate.context.scope);
                },
                fnInitComplete: function(oSettings, data) {
                    if (!!bindings.url.value) {
                        angular.element('table thead th', parent_container)
                            .unbind('click.sorting')
                            .unbind('click.DT');
                    }
                }
            };

            // Add table ordering
            _.each(element.columns, function(column, index) {
                if (column.sort_by && column.sort_dir) {
                    // Get column sort by
                    var columnSortBy = $delegate.isBinding(column.sort_by)
                        ? column.sort_by.binding.join('')
                        : column.sort_by.content;

                    // Get column sort direction
                    var bindingSortDir = $delegate.getBinding(column.sort_dir, context_path);

                    var columnSortDir = bindingSortDir.kind === 'binding'
                        ? $parse(bindingSortDir.value)($delegate.context.scope)
                        : $interpolate(bindingSortDir.value)($delegate.context.scope);

                    if (tableOptions.isVirtual && !tableOptions.virtualSortBy && !tableOptions.virtualSortDir) {
                        tableOptions.virtualSortBy = columnSortBy;
                        tableOptions.virtualSortDir = columnSortDir;
                    }

                    tableOptions.order.push([index, columnSortDir]);
                }
            });

            // Get header
            var elemHeader = angular.element('thead > tr', elemTable);

            // Add table captions
            _.map(element.columns, function(column, columnIndex) {
                // Add caption cell
                var elemCell = angular.element('<th></th>');
                elemHeader[0].appendChild(elemCell[0]);

                // Get column bindings
                var bindingCaption = $delegate.getBinding(column.caption, context_path);

                // Get option to sort by
                var substituteColumnSortBy = $delegate.isBinding(column.sort_by)
                    ? column.sort_by.binding.join('')
                    : undefined;

                // Render caption
                if (column.caption_template) {
                    reactive.render(elemCell[0], column.caption_template, context_path, context);
                } else {
                    var columnCaption = bindingCaption.kind === 'binding'
                        ? ['{{ ', bindingCaption.value , ' }}'].join('')
                        : (bindingCaption.value + '');

                    elemCell.html(columnCaption);
                }

                // Add sort handler
                elemCell.on('click', function(event) {
                    if (!tableOptions.isVirtual || angular.element(event.target).hasClass('sorting_disabled'))
                        return;

                    var settings = oTable.fnSettings();
                    var lastSort = settings.aaSorting[0];
                    var prevOrderDir = lastSort === undefined ? 'asc' : lastSort[1];
                    var orderDir = prevOrderDir === 'asc' ? 'desc' : 'asc';

                    var requestParams = {
                        start: tableOptions.virtualStart,
                        count: tableOptions.virtualLength,
                        order_by: substituteColumnSortBy,
                        order_dir: orderDir
                    };

                    settings.aaSorting = [[columnIndex, orderDir]];

                    var dataUrl = $interpolate(bindings.url.value)($delegate.context.scope);

                    api.requestRemoteApi('GET', instance.active.id, dataUrl, requestParams, {}, function(reply) {
                        if (reply.result === false || !oTable)
                            return;

                        utils.appendArray(contextTable, reply, true);
                        oTable.fnClearTable();
                        oTable.fnAddData(contextTable);
                    });
                });
            });

            // TODO: refactor this code
            // ---------------------------------------
            // -------- UGLY CODE STARTS HERE --------
            // ---------------------------------------
            var pagingControls = {
                nFirst: document.createElement('span'),
                nPrevious: document.createElement('span'),
                nNext: document.createElement('span'),
                nLast: document.createElement('span'),
                nInput: document.createElement('input'),
                nPage: document.createElement('span'),
                nOf: document.createElement('span')
            };

            var fnCompare = function (value1, value2, sign) {
                var val1 = value1.trim().toLocaleLowerCase();
                var val2 = value2.trim().toLocaleLowerCase();

                if ($.isNumeric(val1) && $.isNumeric(val2)) {
                    val1 = parseFloat(val1);
                    val2 = parseFloat(val2);
                }

                return val1 === val2 ? 0 : (val1 > val2 ? 1 : -1) * sign;
            };

            var fnInfoCallback = function() {
                if (oTable) {
                    var callbackFrom = tableOptions.virtualStart + 1;
                    var callbackTo = (tableOptions.virtualStart + oTable.fnSettings().fnRecordsTotal());
                    var callbackOf = tableOptions.virtualTotalCount;

                    return tableOptions.virtualTotalCount
                        ? ['Showing ', callbackFrom, ' to ', callbackTo, ' of ', callbackOf, ' entries'].join('')
                        : '';
                }
            };

            var fnGetLastSortParam = function(oSettings) {
                var orderByColIndex = oSettings.aLastSort.length > 0 ? oSettings.aLastSort[0].col : -1;
                var orderBy = orderByColIndex !== -1 ? oSettings.aoColumns[orderByColIndex].sortBy : undefined;
                var orderDir = oSettings.aLastSort.length > 0 ? oSettings.aLastSort[0].dir : undefined;

                return [orderBy, orderDir];
            };

            var fnVirtualPageChange = function(oSettings) {
                var sortParam = fnGetLastSortParam(oSettings);
                fnRequest(sortParam[0], sortParam[1], tableOptions.virtualStart, tableOptions.virtualLength);
            };

            var fnRequest = function(orderBy, orderDir, startElement, countElement) {
                var requestParams = { start: startElement, count: countElement };

                if (orderBy !== undefined && orderDir !== undefined) {
                    requestParams.order_by = orderBy;
                    requestParams.order_dir = orderDir;
                }

                var dataUrl = $interpolate(bindings.url.value)($delegate.context.scope);

                api.requestRemoteApi('GET', instance.active.id, dataUrl, requestParams, {}, function(reply) {
                    if (reply.result === false || !oTable)
                        return;

                    utils.appendArray(contextTable, reply, true);
                    var dataUrlCount = $interpolate(tableOptions.virtualCountUrl)($delegate.context.scope);

                    api.requestRemoteApi('GET', instance.active.id, dataUrlCount, {}, {}, function(reply) {
                        var totalCount = parseInt(reply);

                        if (!isNaN(totalCount))
                            tableOptions.virtualTotalCount = totalCount;
                        else
                            tableOptions.virtualTotalCount = tableOptions.virtualLength;

                        oTable.fnClearTable();
                        contextTable.length > 0 && oTable.fnAddData(contextTable);
                    });
                });
            };

            var fnInitPagingControl = function(oSettings, nPaging) {
                pagingControls.nFirst.innerHTML = '««';
                pagingControls.nPrevious.innerHTML = '«';
                pagingControls.nNext.innerHTML = '»';
                pagingControls.nLast.innerHTML = '»»';

                pagingControls.nFirst.className = 'paginate_button first disabled';
                pagingControls.nPrevious.className = 'paginate_button previous disabled';
                pagingControls.nNext.className = 'paginate_button next';
                pagingControls.nLast.className = 'paginate_button last';
                pagingControls.nOf.className = 'paginate_of';
                pagingControls.nPage.className = 'paginate_page';
                pagingControls.nInput.className = 'paginate_input';

                if (oSettings.sTableId !== '') {
                    nPaging.setAttribute('id', oSettings.sTableId + '_paginate');
                    pagingControls.nPrevious.setAttribute('id', oSettings.sTableId + '_previous');
                    pagingControls.nPrevious.setAttribute('id', oSettings.sTableId + '_previous');
                    pagingControls.nNext.setAttribute('id', oSettings.sTableId + '_next');
                    pagingControls.nLast.setAttribute('id', oSettings.sTableId + '_last');
                }

                pagingControls.nInput.type = 'text';
                pagingControls.nPage.innerHTML = ' Page ';

                angular.element(nPaging).append([
                    pagingControls.nFirst,
                    pagingControls.nPrevious,
                    pagingControls.nPage,
                    pagingControls.nInput,
                    pagingControls.nOf,
                    pagingControls.nNext,
                    pagingControls.nLast
                ]).addClass('non-selectable').css('padding-right', '15px');
            };

            var fnSetPagingButtonState = function(state) {
                switch (state) {
                    case 'SinglePage':
                        angular.element(pagingControls.nFirst).addClass('disabled');
                        angular.element(pagingControls.nPrevious).addClass('disabled');
                        angular.element(pagingControls.nNext).addClass('disabled');
                        angular.element(pagingControls.nLast).addClass('disabled');
                        break;
                    case 'FirstPage':
                        angular.element(pagingControls.nFirst).addClass('disabled');
                        angular.element(pagingControls.nPrevious).addClass('disabled');
                        angular.element(pagingControls.nNext).removeClass('disabled');
                        angular.element(pagingControls.nLast).removeClass('disabled');
                        break;
                    case 'LastPage':
                        angular.element(pagingControls.nFirst).removeClass('disabled');
                        angular.element(pagingControls.nPrevious).removeClass('disabled');
                        angular.element(pagingControls.nNext).addClass('disabled');
                        angular.element(pagingControls.nLast).addClass('disabled');
                        break;
                    case 'MiddlePage':
                        angular.element(pagingControls.nFirst).removeClass('disabled');
                        angular.element(pagingControls.nPrevious).removeClass('disabled');
                        angular.element(pagingControls.nNext).removeClass('disabled');
                        angular.element(pagingControls.nLast).removeClass('disabled');
                        break;
                }
            };

            var fnSimplePagingInit = function(oSettings, nPaging, fnCallbackDraw) {
                fnInitPagingControl(oSettings, nPaging);

                if (oSettings.fnRecordsDisplay() <= oSettings._iDisplayLength)
                    fnSetPagingButtonState('SinglePage');

                angular.element(pagingControls.nFirst).click(function () {
                    var iCurrentPage = Math.ceil(oSettings._iDisplayStart / oSettings._iDisplayLength) + 1;

                    if (iCurrentPage != 1) {
                        oSettings.oApi._fnPageChange(oSettings, 'first');
                        fnCallbackDraw(oSettings);
                        fnSetPagingButtonState('FirstPage');
                    }
                });

                angular.element(pagingControls.nPrevious).click(function () {
                    var iCurrentPage = Math.ceil(oSettings._iDisplayStart / oSettings._iDisplayLength) + 1;

                    if (iCurrentPage != 1) {
                        oSettings.oApi._fnPageChange(oSettings, 'previous');
                        fnCallbackDraw(oSettings);

                        if (iCurrentPage == 2) {
                            angular.element(pagingControls.nFirst).addClass('disabled');
                            angular.element(pagingControls.nPrevious).addClass('disabled');
                        }

                        angular.element(pagingControls.nNext).removeClass('disabled');
                        angular.element(pagingControls.nLast).removeClass('disabled');
                    }
                });

                angular.element(pagingControls.nNext).click(function () {
                    var iCurrentPage = Math.ceil(oSettings._iDisplayStart / oSettings._iDisplayLength) + 1;

                    if (iCurrentPage != Math.ceil((oSettings.fnRecordsDisplay() / oSettings._iDisplayLength))) {
                        oSettings.oApi._fnPageChange(oSettings, 'next');
                        fnCallbackDraw(oSettings);

                        if (iCurrentPage == (Math.ceil((oSettings.fnRecordsDisplay() - 1) / oSettings._iDisplayLength) - 1)) {
                            angular.element(pagingControls.nNext).addClass('disabled');
                            angular.element(pagingControls.nLast).addClass('disabled');
                        }

                        angular.element(pagingControls.nFirst).removeClass('disabled');
                        angular.element(pagingControls.nPrevious).removeClass('disabled');
                    }
                });

                angular.element(pagingControls.nLast).click(function () {
                    var iCurrentPage = Math.ceil(oSettings._iDisplayStart / oSettings._iDisplayLength) + 1;

                    if (iCurrentPage != Math.ceil((oSettings.fnRecordsDisplay() / oSettings._iDisplayLength))) {
                        oSettings.oApi._fnPageChange(oSettings, 'last');
                        fnCallbackDraw(oSettings);
                        fnSetPagingButtonState('LastPage');
                    }
                });

                angular.element(pagingControls.nInput).keyup(function (e) {
                    if (oSettings.fnRecordsDisplay() <= oSettings._iDisplayLength) {
                        this.value = 1;
                        return;
                    }

                    if (e.which == 38 || e.which == 39)
                        this.value++;
                    else if ((e.which == 37 || e.which == 40) && this.value > 1)
                        this.value--;

                    if (this.value === '' || this.value.match(/[^0-9]/)) {
                        this.value = this.value.replace(/[^\d]/g, '');
                        if (this.value == '') this.value = 1;
                    }

                    var iNewStart = oSettings._iDisplayLength * (this.value - 1);
                    if (iNewStart < 0)
                        iNewStart = 0;

                    if (iNewStart > oSettings.fnRecordsDisplay())
                        iNewStart = (Math.ceil((oSettings.fnRecordsDisplay() - 1) / oSettings._iDisplayLength) - 1) * oSettings._iDisplayLength;

                    if (iNewStart === 0)
                        fnSetPagingButtonState('FirstPage');
                    else
                        if (iNewStart == ((Math.ceil((oSettings.fnRecordsDisplay() - 1) / oSettings._iDisplayLength) - 1) * oSettings._iDisplayLength))
                            fnSetPagingButtonState('LastPage');
                        else
                            fnSetPagingButtonState('MiddlePage');

                    oSettings._iDisplayStart = iNewStart;
                    fnCallbackDraw(oSettings);
                });

                // Take the brutal approach to cancelling text selection
                angular.element('span', nPaging).bind('mousedown', function () {
                    return false;
                });

                angular.element('span', nPaging).bind('selectstart', function () {
                    return false;
                });
            };

            var fnSimplePagingUpdate = function(oSettings) {
                if (!oSettings.aanFeatures.p)
                    return;

                var iPages = Math.ceil(oSettings.fnRecordsDisplay() / oSettings._iDisplayLength);
                var iCurrentPage = Math.ceil(oSettings._iDisplayStart / oSettings._iDisplayLength) + 1;
                var an = oSettings.aanFeatures.p;

                for (var i = 0, iLen = an.length; i < iLen; i++) {
                    var spans = an[i].getElementsByTagName('span');
                    var inputs = an[i].getElementsByTagName('input');

                    if (iPages > 0) {
                        spans[3].innerHTML = ' of ' + iPages;
                        inputs[0].value = iCurrentPage;
                    } else {
                        spans[3].innerHTML = ' of ' + 0;
                        inputs[0].value = 0;
                    }
                }
            };

            var fnVirtualPagingInit = function(oSettings, nPaging) {
                fnInitPagingControl(oSettings, nPaging);

                angular.element(pagingControls.nFirst).click(function() {
                    tableOptions.virtualStart = 0;
                    fnSetPagingButtonState('FirstPage');
                    fnVirtualPageChange(oSettings);
                });

                angular.element(pagingControls.nPrevious).click(function() {
                    tableOptions.virtualStart -= tableOptions.virtualLength;

                    if (tableOptions.virtualStart < 0) {
                        tableOptions.virtualStart = 0;
                        fnSetPagingButtonState('FirstPage');
                        return;
                    }

                    fnVirtualPageChange(oSettings);
                });

                angular.element(pagingControls.nNext).click(function() {
                    var oldValue = tableOptions.virtualStart;
                    tableOptions.virtualStart += tableOptions.virtualLength;

                    if (tableOptions.virtualStart > tableOptions.virtualTotalCount) {
                        tableOptions.virtualStart = oldValue;
                        fnSetPagingButtonState('LastPage');
                        return;
                    }

                    fnVirtualPageChange(oSettings);
                });

                angular.element(pagingControls.nLast).click(function() {
                    var lastPage = Math.ceil((tableOptions.virtualTotalCount - 1) / tableOptions.virtualLength);
                    tableOptions.virtualStart = (lastPage - 1) * tableOptions.virtualLength;
                    fnSetPagingButtonState('LastPage');
                    fnVirtualPageChange(oSettings);
                });

                angular.element(pagingControls.nInput).keyup(function (e) {
                    if (e.which == 38 || e.which == 39)
                        this.value++;
                    else if ((e.which == 37 || e.which == 40) && this.value > 1)
                        this.value--;

                    if (this.value === '' || this.value.match(/[^0-9]/)) {
                        this.value = this.value.replace(/[^\d]/g, '');
                        if (this.value == '')
                            this.value = 1;
                    }

                    if (this.value < 1)
                        this.value = 1;

                    tableOptions.virtualStart = tableOptions.virtualLength * (this.value - 1);

                    if (tableOptions.virtualStart > tableOptions.virtualTotalCount) {
                        var lastPage = Math.ceil((tableOptions.virtualTotalCount - 1) / tableOptions.virtualLength);
                        this.value = lastPage;
                        tableOptions.virtualStart = (lastPage - 1) * tableOptions.virtualLength;
                    }

                    fnVirtualPageChange(oSettings);
                });
            };

            var fnVirtualPagingUpdate = function(oSettings) {
                var an = oSettings.aanFeatures.p;
                var iPages = Math.ceil(tableOptions.virtualTotalCount / tableOptions.virtualLength);
                var iCurrentPage = Math.ceil(tableOptions.virtualStart / tableOptions.virtualLength) + 1;

                for (var i = 0, iLen = an.length; i < iLen; i++) {
                    if (iPages < 1) {
                        angular.element(an[i]).hide();
                        continue;
                    }

                    angular.element(an[i]).show();
                    var inputs = an[i].getElementsByTagName('input');
                    var spans = an[i].getElementsByTagName('span');

                    if (tableOptions.virtualStart === 0) {
                        angular.element(spans[0]).addClass('disabled');
                        angular.element(spans[1]).addClass('disabled');
                    } else {
                        angular.element(spans[0]).removeClass('disabled');
                        angular.element(spans[1]).removeClass('disabled');
                    }

                    if (tableOptions.virtualStart + tableOptions.virtualLength >= tableOptions.virtualTotalCount) {
                        angular.element(spans[4]).addClass('disabled');
                        angular.element(spans[5]).addClass('disabled');
                    } else {
                        angular.element(spans[4]).removeClass('disabled');
                        angular.element(spans[5]).removeClass('disabled');
                    }

                    if (iPages > 0) {
                        spans[3].innerHTML = ' of ' + iPages;
                    } else {
                        spans[3].innerHTML = ' of 1';
                    }

                    inputs[0].value = iCurrentPage;
                }
            };

            jQuery.fn.dataTableExt.oPagination.full_numbers = {
                fnInit: function (oSettings, nPaging, fnCallbackDraw) {
                    fnSimplePagingInit(oSettings, nPaging, fnCallbackDraw);
                },
                fnUpdate: function (oSettings) {
                    fnSimplePagingUpdate(oSettings);
                }
            };

            jQuery.fn.dataTableExt.oPagination.two_button = {
                fnInit: function(oSettings, nPaging) {
                    fnVirtualPagingInit(oSettings, nPaging);
                },
                fnUpdate: function(oSettings) {
                    fnVirtualPagingUpdate(oSettings);
                }
            };

            jQuery.fn.dataTable.ext.order['zeus'] = function(settings, col) {
                return this.api().column(col, { order: 'index' }).nodes().map(function (td, i) {
                    return angular.element('.sort-by', td).text() || angular.element(td).text();
                });
            };

            jQuery.fn.dataTableExt.oSort['zeus-desc'] = function(x, y) {
                return fnCompare(x, y, -1);
            };

            jQuery.fn.dataTableExt.oSort['zeus-asc'] = function(x, y) {
                return fnCompare(x, y, 1);
            };

            if (bindings.url.value) {
                tableOptions.sPaginationType = 'two_button';
                tableOptions.fnInfoCallback = fnInfoCallback;

                var urlParts = bindings.url.value.split(/\?/g);
                urlParts.splice(1, 0, '/count?');
                tableOptions.virtualCountUrl = urlParts.join('');

                fnRequest(tableOptions.virtualSortBy, tableOptions.virtualSortDir, 0, tableOptions.virtualLength);
            } else {
                tableOptions.data = contextTable;
                tableOptions.sPaginationType = 'full_numbers';
            }

            // Initialize DataTable
            oTable = elemTable.dataTable(tableOptions);

            oTable.on('length.dt', function (e, settings, len) {
                if (!tableOptions.isVirtual) {
                    if (settings.fnRecordsDisplay() <= settings._iDisplayLength)
                        fnSetPagingButtonState('SinglePage');
                    else
                        fnSetPagingButtonState('FirstPage');
                } else {
                    tableOptions.virtualStart = 0;
                    tableOptions.virtualLength = len;

                    var sortParam = fnGetLastSortParam(settings);
                    fnRequest(sortParam[0], sortParam[1], tableOptions.virtualStart, tableOptions.virtualLength);
                }
            });

            if (bindings.url.value) {
                // Watch for data URL changes
                var modelUrl = $interpolate(bindings.url.value);

                $delegate.context.scope.$watch(function() {
                    return modelUrl($delegate.context.scope);
                }, function(new_val, old_val) {
                    if (new_val && !angular.equals(new_val, old_val))
                        fnRequest(tableOptions.virtualSortBy, tableOptions.virtualSortDir, tableOptions.virtualStart, tableOptions.virtualLength);
                }, true);
            } else {
                // Define initial render flag
                var initiallyRendered = false;
                var modelRows = $parse(bindings.rows.value);

                // Watch data source change
                $delegate.context.scope.$watch(function() {
                    return modelRows($delegate.context.scope);
                }, function(new_val, old_val) {
                    var shouldRender = initiallyRendered ? !angular.equals(new_val, old_val) : true;

                    if (!(shouldRender && angular.isArray(new_val)))
                        return;

                    oTable.fnClearTable();

                    if (new_val.length > 0) {
                        oTable.fnAddData(new_val);

                        $timeout(function() {
                            tableOptions.order.length > 0 && oTable.fnSort(tableOptions.order);
                        }, 0);
                    }

                }, true);
            }

            // ---------------------------------------
            // --------- UGLY CODE ENDS HERE ---------
            // ---------------------------------------
        });

        return $delegate;
    });
});
