angular.module('zeusApp').constant('Menu', [
    {
        regexp: '^\/admin\/dashboard$',
        title: 'Dashboard',
        url: '/admin/dashboard',
        icon: 'dashboard.png',
        items: []
    },
    {
        regexp: '^\/admin\/settings(\/(.*)?)?$',
        title: 'Administration',
        url: '/admin/settings',
        icon: 'page-layouts.png',
        restrict: { admin: true },
        items: [
            {
                regexp: '^\/admin\/settings\/users(\/(.*)?)?$',
                title: 'Users',
                url: '/admin/settings/users',
                icon: 'form-elements.png',
                restrict: { admin: ['manage_users'] },
                items: []
            },
            {
                regexp: '^\/admin\/settings\/instances(\/(.*)?)?$',
                title: 'Instances',
                url: '/admin/settings/instances',
                icon: 'page-layouts.png',
                restrict: { admin: ['manage_instances'] },
                items: []
            }
        ]
    }
]);
