angular.module('zeusApp').filter('cssColor', function() {
    return function(input, scope) {
        switch (typeof input) {
            case 'number':
                return 'rgb(%red%, %green%, %blue%)'
                    .replace(/%red%/g, (input >> 16) & 0xff)
                    .replace(/%green%/g, (input >> 8) & 0xff)
                    .replace(/%blue%/g, input & 0xff);
            case 'string':
                return input;
            default:
                return 'inherit';
        }
    }
});
