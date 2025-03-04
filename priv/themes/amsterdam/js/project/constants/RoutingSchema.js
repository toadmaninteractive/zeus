angular.module('zeusApp').constant('RoutingSchema', {
    not_authenticated: {
        routes: [
            { regexp: '^\/admin\/login$', index: 'login.html', layout: '' }
        ],
        otherwise: { redirectTo: '/admin/login' }
    },
    authenticated: {
        routes: [
            { regexp: '^\/admin\/login$', redirectTo: '/admin/dashboard' },
            { regexp: '^\/admin\/dashboard$', index: 'layout.html', layout: 'dashboard.html' },
            { regexp: '^\/admin\/settings$', index: 'layout.html', layout: 'admin_settings.html', restrict: { admin: true } },
            { regexp: '^\/admin\/settings\/instances(\/(.*)?)?$', index: 'layout.html', layout: 'instances_view.html', restrict: { admin: ['manage_instances'] } },
            { regexp: '^\/admin\/settings\/users(\/(.*)?)?$', index: 'layout.html', layout: 'users_view.html', restrict: { admin: ['manage_users'] } },
            { regexp: '^\/admin(\/(.*)?)?$', index: 'layout.html', layout: '404.html' },
            { regexp: '^\/[a-zA-Z0-9]{1}[a-zA-Z0-9_]*(\/(.*)?)?$', index: 'layout.html', layout: 'instance_link.html' }
        ],
        otherwise: { redirectTo: '/admin/dashboard' }
    }
});
