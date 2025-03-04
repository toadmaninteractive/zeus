angular.module('zeusApp').constant('RenderSchema', {
    common: {
        events: ['on_load', 'on_hover'],
        bindings: ['visible']
    },
    elements: {
        horizontal: {
            layouts: ['items']
        },
        vertical: {
            layouts: ['items']
        },
        listing: {
            bindings: ['items', 'show_separator'],
            layouts: ['item_template']
        },
        label: {
            bindings: ['text', 'color']
        },
        block: {
            bindings: ['caption', 'content'],
            layouts: ['caption_template', 'content_template']
        },
        table: {
            bindings: ['rows', 'pagination', 'url', 'compact'],
            custom: [
                {
                    key: 'columns',
                    bindings: ['caption', 'content', 'align', 'sort_by', 'sort_dir'],
                    layouts: ['caption_template', 'content_template']
                }
            ]
        },
        textedit: {
            bindings: ['text', 'placeholder', 'kind', 'multiline']
        },
        button: {
            events: ['on_click'],
            bindings: ['text']
        },
        link: {
            events: ['on_click'],
            bindings: ['text']
        },
        layout: {
            bindings: ['layout_url']
        },
        image: {
            events: ['on_click'],
            bindings: ['source', 'source_type', 'scale']
        },
        chart: {
            bindings: ['caption', 'chart_type', 'source', 'series', 'flat']
        },
        imagebutton: {
            events: ['on_click'],
            bindings: ['icon', 'tooltip']
        },
        checkbox: {
            bindings: ['checked', 'text', 'color'],
            events: ['on_change']
        },
        combobox: {
            bindings: ['items', 'caption', 'selected'],
            events: ['on_change']
        },
        inplace_textedit: {
            bindings: ['text'],
            events: ['on_change']
        },
        separator: {
            bindings: []
        },
        html: {
            bindings: ['content']
        },
        tabs: {
            custom: [
                {
                    key: 'items',
                    bindings: ['layout_url', 'title', 'icon']
                }
            ]
        },
        radiogroup: {
            bindings: ['value', 'vertical_order'],
            custom: [
                {
                    key: 'items',
                    bindings: ['caption', 'value'],
                    layouts: ['template']
                }
            ]
        },
        datetimepicker: {
            bindings: ['date_time', 'mode']
        },
        timeinterval: {
            bindings: ['interval', 'mode']
        },
        property_grid: {
            bindings: ['collapsed', 'value']
        }
    }
});
