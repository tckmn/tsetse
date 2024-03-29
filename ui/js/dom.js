m.dom = (function() {

    var hotkeys = [
        '1234567890',
        'qwertyuiop',
        'asdfghjkl;',
        'zxcvbnm,./'
    ];

    var cells = [];
    var selected = [];
    var keylisten = {};

    return {

        resize: function() {
            var rect = m.e.wallwrap.getBoundingClientRect();
            m.e.wall.style.maxWidth = m.conf.get('square') ? rect.height + 'px' : '';
            m.e.wall.style.maxHeight = m.conf.get('square') ? rect.width + 'px' : '';
        },

        el: function(name, props, issvg) {
            var el = issvg ?
                document.createElementNS('http://www.w3.org/2000/svg', name) :
                document.createElement(name);
            if (props) for (prop in props) {
                if (prop === 'text') el.appendChild(document.createTextNode(props.text));
                else if (prop === 'children') props.children.forEach(ch => {
                    if (typeof ch === 'string') el.appendChild(document.createTextNode(ch));
                    else el.appendChild(ch);
                });
                else if (prop.slice(0,2) === 'on') el.addEventListener(prop.slice(2), props[prop]);
                else el.setAttribute(prop[0] === '_' ?
                        prop.slice(1) :
                        prop.replace(/[A-Z]/g, m => '-'+m.toLowerCase()),
                    props[prop]);
            }
            return el;
        },

        svgel: function(name, props) {
            return this.el(name, props, true);
        },

        clr: function(el) {
            while (el.firstChild) el.removeChild(el.firstChild);
        },

        toast: function(msg) {
            // can't use onclick here because they're mutually recursive, lol
            var toast = m.dom.el('div', {
                class: 'toast',
                text: msg
            }), close = () => {
                if (toast) document.body.removeChild(toast);
                toast = undefined;
            };
            toast.addEventListener('click', close);
            document.body.appendChild(toast);
            setTimeout(close, Math.max(5000, msg.length*200));
        },

        addCell: function(content, idx, autosubmit) {
            var perrow = m.conf.get('rownum'),
                hotkey = (hotkeys[idx/perrow|0]||'')[idx%perrow] || '';

            var cell = document.createElement('div');
            cell.dataset.idx = idx;
            cell.classList.add('cell');
            cell.appendChild(content);
            cell.appendChild(this.el('span', {
                text: hotkey, class: 'hotkey'
            }));
            m.e.wall.appendChild(cell);
            cells.push(cell);

            var fn = e => {
                if (e) e.preventDefault();
                var idx = selected.indexOf(+cell.dataset.idx);
                if (cell.classList.toggle('selected')) {
                    if (idx === -1) selected.push(+cell.dataset.idx);
                } else {
                    if (idx !== -1) selected.splice(idx, 1);
                }
                this.submitCells(e.shiftKey ? Infinity : autosubmit);
            };
            cell.addEventListener('mousedown', fn);
            cell.addEventListener('touchstart', fn);
            keylisten[hotkey] = fn;
        },

        eachCell: function(f) {
            return cells.forEach(f);
        },

        submitCells: function(reqnum) {
            if (selected.length >= (reqnum || 1)) {
                Array.from(document.getElementsByClassName('selected'))
                    .forEach(g => g.classList.remove('selected'));
                m.net.send('Claim', { idxs: selected });
                selected = [];
            }
        },

        clearCells: function() {
            this.clr(m.e.wall);
            cells = [];
            selected = [];
            keylisten = {};
        },

        _onload: function() {
            // i guess random listeners will go here
            window.addEventListener('resize', this.resize);
            window.addEventListener('keydown', e => {
                if (e.key == 'Enter') this.submitCells();
                else if (keylisten[e.key]) keylisten[e.key](e);
            });
        }
    };

})();
