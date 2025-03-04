angular.module('zeusApp').constant('Breadcrumbs', [
    {
        regexp: '^\/admin\/dashboard$',
        items: [
            { title: 'Dashboard', url: '' }
        ]
    },
    {
        regexp: '^\/admin\/settings$',
        items: [
            { title: 'Administration', url: '' }
        ]
    },
    {
        regexp: '^\/admin\/settings\/instances(\/(.*)?)?$',
        items: [
            { title: 'Instances', url: '' }
        ]
    },
    {
        regexp: '^\/admin\/settings\/users(\/(.*)?)?$',
        items: [
            { title: 'Users', url: '' }
        ]
    },
    {
        regexp: '^\/admin(\/(.*)?)?$',
        items: [
            { title: 'Error 404: Not Found', url: '' }
        ]
    },
    {
        regexp: '^\/[a-zA-Z0-9]{1}[a-zA-Z0-9_]*(\/(.*)?)?$',
        items: [
            { title: 'Instance', url: '', remote: true }
        ]
    }
]);
