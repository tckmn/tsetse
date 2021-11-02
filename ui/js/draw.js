m.draw = (function() {

    return {

        create: viewBox => ({
            polys: [],
            poly: function(pts, props) {
                if (props && props.fancy) {
                    this.polys.push(pts);
                    delete props.fancy;
                }
                this.svg.appendChild(m.dom.svgel('path', {
                    d: 'M ' + pts.map(a => a.join(' ')).join(' L ') + ' Z',
                    ...props
                }));
            },
            fancy: function(outer, inner) {
                this.polys.forEach(p => {
                    this.poly(p, {
                        fill: 'none', stroke: '#fff', strokeWidth: outer,
                        strokeLinejoin: 'round', strokeLinecap: 'round'
                    });
                });
                this.polys.forEach(p => {
                    this.poly(p, {
                        fill: 'none', stroke: '#000', strokeWidth: inner,
                        strokeLinejoin: 'round', strokeLinecap: 'round'
                    });
                });
            },
            svg: m.dom.svgel('svg', { _viewBox: viewBox })
        })

    };

})();
