m.draw = (function() {

    return {

        create: (viewBox, svgProps) => ({
            group: function(props) {
                this.activeGroup = m.dom.svgel('g', props);
                this.svg.appendChild(this.activeGroup);
            },
            el: function(name, props) {
                (this.activeGroup || this.svg).appendChild(m.dom.svgel(name, props));
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
                this.polys = [];
            },
            fidget: function(n, r, parity, props) {
                var offset = 0;
                if (props && props.offset) {
                    offset = props.offset;
                    delete props.offset;
                }
                m.util.range(n, i => {
                    var p = [Math.sin(2*Math.PI/n*(i+offset))*r,
                        Math.cos(2*Math.PI/n*(i+offset))*r];
                    this.el('path', {
                        d: `M 0 0 A ${r/2} ${r/2} 0 0 ${parity ? 0 : 1} ${p.join(' ')}`,
                        fill: 'none', ...props,
                        strokeLinejoin: 'round', strokeLinecap: 'round'
                    });
                });
            },
            svg: m.dom.svgel('svg', { _viewBox: viewBox, ...svgProps })
        })

    };

})();
