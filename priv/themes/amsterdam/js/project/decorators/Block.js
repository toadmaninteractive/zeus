angular.module('zeusApp').config(function($provide) {
    $provide.decorator('reactive', function($delegate, $parse) {
        $delegate.addRenderer('block', function(parent_container, element, context_path, context) {
            // Update parent container style
            parent_container.style.display = 'block';

            // Get bindings and layouts
            var bindings = $delegate.getBindings(element, context_path);
            var layouts = $delegate.getLayouts(element, context_path);

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

            // Add block content
            var elemBlockContent = angular.element('<div class="row-fluid body zeus-block-content"></div>');
            elemBlock[0].appendChild(elemBlockContent[0]);

            // Render caption
            if (layouts.caption_template) {
                // Render template
                var captionContextPath = bindings.caption.kind === 'binding' ? bindings.caption.value : context_path;
                var captionContext = bindings.caption.kind === 'binding' ? $parse(bindings.caption.value)($delegate.context.scope) : context;
                $delegate.render(elemInnerHeading[0], layouts.caption_template,  captionContextPath, captionContext);
            } else {
                // Substitute text
                var substituteCaptionText = bindings.caption.kind === 'binding'
                    ? ['<span ng-bind-html="', bindings.caption.value, '"></span>'].join('')
                    : bindings.caption.value;

                elemInnerHeading.append(substituteCaptionText);
            }

            // Render content
            if (layouts.content_template) {
                // Render template
                var contentContextPath = bindings.content.kind === 'binding' ? bindings.content.value : context_path;
                var contentContext = bindings.content.kind === 'binding' ? $parse(bindings.content.value)($delegate.context.scope) : context;
                $delegate.render(elemBlockContent[0], layouts.content_template,  contentContextPath, contentContext);
            } else {
                // Substitute text
                var substituteContentText = bindings.content.kind === 'binding'
                    ? ['<span ng-bind-html="', bindings.content.value, '"></span>'].join('')
                    : bindings.content.value;

                elemBlockContent.append(substituteContentText);
            }
        });

        return $delegate;
    });
});
