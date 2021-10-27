m.dom = (function() {

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

        cells: [],
        addCell: function(content, idx) {
            var cell = document.createElement('div');
            cell.dataset.idx = idx;
            cell.classList.add('cell');
            cell.appendChild(content);
            m.e.wall.appendChild(cell);
            this.cells.push(cell);

            var fn = e => {
                e.preventDefault();
                if (true) {
                    cell.classList.toggle('selected');
                    var guesses = Array.from(document.getElementsByClassName('selected'));
                    // TODO not hardcode
                    if (guesses.length === 5) {
                        guesses.forEach(g => g.classList.remove('selected'));
                        m.net.send('Claim', {
                            idxs: guesses.map(g => +g.dataset.idx)
                        });
                    }
                }
            };
            cell.addEventListener('mousedown', fn);
            cell.addEventListener('touchstart', fn);
        },

        _onload: function() {
            window.addEventListener('resize', this.resize);
        }
    };

})();
