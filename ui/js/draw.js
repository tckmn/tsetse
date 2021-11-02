m.draw = (function() {

    return {

        create: (viewBox, svgProps) => ({
            el: function(name, props) {
                this.svg.appendChild(m.dom.svgel(name, props));
            },
            line: function(pts, props) {
                this.el('path', {
                    d: 'M ' + pts.map(a => a.join(' ')).join(' L '),
                    strokeLinejoin: 'round', strokeLinecap: 'round',
                    fill: 'none', ...props
                });
            },
            polys: [],
            poly: function(pts, props) {
                if (props && props.fancy) {
                    this.polys.push(pts);
                    delete props.fancy;
                }
                this.el('path', {
                    d: 'M ' + pts.map(a => a.join(' ')).join(' L ') + ' Z',
                    strokeLinejoin: 'round', strokeLinecap: 'round',
                    ...props
                });
            },
            fancy: function(outer, inner) {
                this.polys.forEach(p => {
                    this.poly(p, {
                        fill: 'none', stroke: '#fff', strokeWidth: outer
                    });
                });
                this.polys.forEach(p => {
                    this.poly(p, {
                        fill: 'none', stroke: '#000', strokeWidth: inner
                    });
                });
            },
            svg: m.dom.svgel('svg', { _viewBox: viewBox, ...svgProps })
        })

    };

})();
