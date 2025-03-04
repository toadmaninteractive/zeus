angular.module('zeusApp').service('growl', function() {
    this.message = function(text, header, kind, lifetime) {
        !lifetime && (lifetime = 5000);

        var kinds = {
            info: 'default',
            success: 'growl-success',
            warning: 'growl-warning',
            error: 'growl-error'
        };

        var theme = kinds[kind] ? kinds[kind] : kinds['info'];

        $.jGrowl('<b>' + text + '</b>', { life: lifetime, theme: theme, header: header || '' });
    };
});
