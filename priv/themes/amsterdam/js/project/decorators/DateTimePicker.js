angular.module('zeusApp').config(function($provide) {
    $provide.decorator('reactive', function($delegate, $parse, $interpolate, $filter) {
        $delegate.addRenderer('datetimepicker', function(parent_container, element, context_path, context) {
            // Get bindings
            var bindings = $delegate.getBindings(element, context_path);

            // Prepare temporary value for date_time substitution
            var auxId = $delegate.generateAuxId();
            var contextPathSubstitute = [$delegate.contextPath, '.data.aux.', auxId].join('');
            $delegate.context.data.aux[auxId] = null;

            // Prepare substitution values
            var substituteDateTime = bindings.date_time.kind === 'binding'
                ? $parse(bindings.date_time.value)($delegate.context.scope)
                : bindings.date_time.value
                    ? $interpolate(bindings.date_time.value)($delegate.context.scope)
                    : bindings.date_time.value;

            // Initialize temporary value
            if (substituteDateTime) {
                switch (bindings.mode.value) {
                    case 'date':
                        $delegate.context.data.aux[auxId] = new Date(substituteDateTime);
                        break;
                    case 'time':
                        $delegate.context.data.aux[auxId] = new Date('1970-01-01T' + substituteDateTime.toString());
                        break;
                    case 'datetime':
                        $delegate.context.data.aux[auxId] = new Date(substituteDateTime);
                        break;
                }
            }

            // Define templates
            var templateDate = '\
                <input \
                    placeholder="YYYY-MM-DD" \
                    class="input-date" \
                    type="text" \
                    reactive-validate \
                    data-validate="date" \
                    data-validate-mode="text" \
                    ng-model="%datetime%" \
                    datepicker-popup="yyyy-MM-dd" \
                    is-utc-today="true" \
                    readonly \
                    show-weeks="false" \
                    starting-day="1" \
                    style="cursor: pointer" />'
                .replace(/%datetime%/, contextPathSubstitute);

            var templateTime = '\
                <input \
                    placeholder="HH:MM:SS" \
                    class="form-control input-time" \
                    type="text" \
                    sy-timepicker-popup="HH:mm:ss" \
                    reactive-validate \
                    data-validate="time" \
                    data-validate-mode="text" \
                    readonly \
                    ng-model="%datetime%" \
                    show-meridian="false" \
                    is-open-prevent="opened" \
                    style="cursor: pointer" />'
                .replace(/%datetime%/, contextPathSubstitute);

            var templateUtc = '<span class="utc" tooltip="{{ %datetime% | date:\'yyyy-MM-ddTHH:mm:ss\' }}"> UTC</span>'
                .replace(/%datetime%/, contextPathSubstitute);

            var template;
            var parseTemplate;

            switch (bindings.mode.value) {
                case 'datetime':
                    template = '<div class="date-time-picker">' + templateDate + templateTime + templateUtc + '</div>';
                    parseTemplate = 'yyyy-MM-ddTHH:mm:ss';
                    break;
                case 'date':
                    template = templateDate + templateUtc;
                    parseTemplate = 'yyyy-MM-dd';
                    break;
                case 'time':
                    template = templateTime + templateUtc;
                    parseTemplate = 'HH:mm:ss';
                    break;
            }

            // Define model
            var modelDateTime = $parse(bindings.date_time.value);

            // Watch temporary model
            $delegate.context.scope.$watch(function() {
                return $delegate.context.data.aux[auxId];
            }, function(new_val) {
                modelDateTime.assign($delegate.context.scope, $filter('date')(new_val, parseTemplate));
            }, true);

            // Add element
            var elemDateTimePicker = angular.element(template);
            parent_container.appendChild(elemDateTimePicker[0]);
        });

        return $delegate;
    });
});
