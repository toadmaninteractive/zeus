angular.module('zeusApp').config(function($provide) {
    $provide.decorator('reactive', function($delegate, $parse) {
        $delegate.addRenderer('timeinterval', function(parent_container, element, context_path, context) {
            // Get bindings
            var bindings = $delegate.getBindings(element, context_path);

            // Prepare temporary value for interval substitution
            var auxId = $delegate.generateAuxId();
            var contextPathSubstitute = [$delegate.contextPath, '.data.aux.', auxId].join('');
            $delegate.context.data.aux[auxId] = null;

            // Prepare substitution values
            var substituteInterval = bindings.interval.kind === 'binding'
                ? $parse(bindings.interval.value)($delegate.context.scope)
                : bindings.interval.value;

            substituteInterval *= bindings.mode.value === 'seconds' ? 1000 : 1;
            var substituteTime = new Date(substituteInterval);

            // Get UTC time
            var utcHours = substituteTime.getUTCHours();
            var utcMinutes = substituteTime.getUTCMinutes();
            var utcSeconds = substituteTime.getUTCSeconds();

            // Update time
            substituteTime.setHours(utcHours);
            substituteTime.setMinutes(utcMinutes);
            substituteTime.setSeconds(utcSeconds);

            // Define template
            var template = '\
                <input \
                    type="text" \
                    class="form-control input-time" \
                    placeholder="HH:MM:SS" \
                    sy-timepicker-popup="HH:mm:ss" \
                    reactive-validate \
                    data-validate="time" \
                    data-validate-mode="text" \
                    ng-model="%time%" \
                    show-meridian="false" \
                    interval />'
                .replace(/%time%/, contextPathSubstitute);

            // Define model
            var modelTime = $parse(bindings.interval.value);

            // Watch temporary model
            $delegate.context.scope.$watch(function() {
                return $delegate.context.data.aux[auxId];
            }, function(new_val) {
                var interval = 0;

                if (new_val !== undefined && new_val !== null) {
                    interval = new_val.getSeconds();
                    interval += new_val.getMinutes() * 60;
                    interval += new_val.getHours() * 60 * 60;
                    interval *= bindings.mode.value === 'milliseconds' ? 1000 : 1;
                }

                modelTime.assign($delegate.context.scope, interval);
            }, true);

            // Add element
            var elemTimeInterval = angular.element(template);
            parent_container.appendChild(elemTimeInterval[0]);
        });

        return $delegate;
    });
});
