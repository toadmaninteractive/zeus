angular.module('zeusApp').config(function($provide) {
    $provide.decorator('reactive', function($delegate, $parse, $interpolate) {
        $delegate.addRenderer('chart', function(parent_container, element, context_path, context) {
            // Update parent container style
            parent_container.style.display = 'block';

            // Get bindings and layouts
            var bindings = $delegate.getBindings(element, context_path);

            // Add block
            var elemBlock = angular.element('<div class="well disperse"></div>');
            parent_container.appendChild(elemBlock[0]);

            // Add block header and its siblings
            var elemBlockHeader = angular.element('<div class="navbar"></div>');
            elemBlock[0].appendChild(elemBlockHeader[0]);
            var elemHeaderInner = angular.element('<div class="navbar-inner"></div>');
            elemBlockHeader[0].appendChild(elemHeaderInner[0]);
            var elemInnerHeading = angular.element('<h5></h5>');
            elemHeaderInner[0].appendChild(elemInnerHeading[0]);

            // Add block content and its siblings
            var elemBlockContent = angular.element('<div class="row-fluid body"></div>');
            elemBlock[0].appendChild(elemBlockContent[0]);
            var elemContentCanvas = angular.element('<canvas style="width: 100% !important;"></canvas>');
            elemBlockContent[0].appendChild(elemContentCanvas[0]);
            var elemLegend = angular.element('<div class="chart-legend"></div>');
            elemBlockContent[0].appendChild(elemLegend[0]);

            // Render caption
            var substituteCaptionText = bindings.caption.kind === 'binding'
                ? ['<span ng-bind-html="', bindings.caption.value, '"></span>'].join('')
                : bindings.caption.value || 'Chart';

            elemInnerHeading.append(substituteCaptionText);

            // Get canvas context
            var canvasContext = elemContentCanvas[0].getContext('2d');
            var chart;

            // Define colors
            var chartColors = [
                { r: 102, g: 205, b: 170 }, // MediumAquamarine
                { r: 106, g: 90, b: 205 },  // SlateBlue
                { r: 205, g: 92, b: 92 },   // IndianRed
                { r: 107, g: 142, b: 35 },  // OliveDrab
                { r: 255, g: 215, b: 0 },   // Gold
                { r: 25, g: 25, b: 112 },   // MidnightBlue
                { r: 47, g: 79, b: 79 },    // DarkSlateGray
                { r: 205, g: 133, b: 63 },  // Peru
                { r: 30, g: 144, b: 255 },  // DodgerBlue
                { r: 138, g: 43, b: 226 }   // BlueViolet
            ];

            var fnMkColor = function(color, opacity) {
                return 'rgba(' + [color.r, color.g, color.b, opacity].join(', ') + ')';
            };

            $delegate.context.scope.$watch(bindings.source.value, function(new_value, old_value) {
                if (!angular.isArray(new_value) || new_value.length < 1)
                    return;

                // Destroy chart
                chart && chart.destroy();

                // Prepare labels
                var labelSrc = !!bindings.flat.value ? new_value : new_value[0];
                var labels = _.map(labelSrc, function(data_set) {
                    return data_set[0];
                });

                var dataSets = [];

                for (var index = 0; index < bindings.series.length; index++) {
                    var label = bindings.series[index].kind === 'binding'
                        ? $parse(bindings.series[index].value)($delegate.context.scope)
                        : $interpolate(bindings.series[index].value)($delegate.context.scope);

                    var isPoint = bindings.chart_type.value === 'point';

                    dataSets.push({
                        label: label,
                        fillColor: isPoint ? 'transparent' : fnMkColor(chartColors[index % chartColors.length], 0.5),
                        strokeColor: isPoint ? 'transparent' : fnMkColor(chartColors[index % chartColors.length], 0.8),
                        highlightFill: isPoint ? 'transparent' : fnMkColor(chartColors[index % chartColors.length], 0.75),
                        highlightStroke: isPoint ? 'transparent' : fnMkColor(chartColors[index % chartColors.length], 1),
                        pointColor: isPoint ? fnMkColor(chartColors[index % chartColors.length], 0.5) : 'transparent',
                        pointStrokeColor: isPoint ? fnMkColor(chartColors[index % chartColors.length], 0.8) : 'transparent',
                        pointHighlightFill: isPoint ? fnMkColor(chartColors[index % chartColors.length], 0.75) : 'transparent',
                        pointHighlightStroke: isPoint ? fnMkColor(chartColors[index % chartColors.length], 1) : 'transparent',
                        data: !!bindings.flat.value
                            ? _.map(new_value, function(data_set) { return data_set[index + 1]; })
                            : _.map(new_value[index], function(data_bit) { return data_bit[1]; })
                    });
                }

                // Render chart
                switch (bindings.chart_type.value) {
                    case 'bar':
                        chart = new Chart(canvasContext).Bar({ labels: labels, datasets: dataSets }, { responsive: true });
                        break;
                    case 'line':
                        chart = new Chart(canvasContext).Line({ labels: labels, datasets: dataSets }, { responsive: true });
                        break;
                    case 'point':
                        chart = new Chart(canvasContext).Line({ labels: labels, datasets: dataSets }, { responsive: true, pointDotRadius: 8 });
                        break;
                }

                // Render legend
                elemLegend[0].innerHTML = chart.generateLegend();
            }, true);
        });

        return $delegate;
    });
});
