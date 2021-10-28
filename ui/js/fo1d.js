m.FO1D = (function() {

    return {

        render: function(card) {
            var svg = m.dom.svgel('svg', {
                _viewBox: '-1 -1 2 2', class: 'grow'
            });

            var x = j => 2*Math.cos(Math.PI*2/10*j),
                y = j => 2*Math.sin(Math.PI*2/10*j);

            m.util.range(10, i => {
                svg.appendChild(m.dom.svgel('path', {
                    d: `M 0 0 L ${x(i)} ${y(i)}`,
                    stroke: '#888', strokeWidth: 0.01
                }));
            });

            card.forEach(c => {
                svg.appendChild(m.dom.svgel('path', {
                    d: `M 0 0 L ${x(c[0])} ${y(c[0])}`,
                    stroke: c[1] ? '#f00' : '#00f', strokeWidth: 0.05
                }));
            });

            svg.appendChild(m.dom.svgel('circle', {
                cx: 0, cy: 0, r: 0.05, fill: '#000'
            }));

            return svg;
        },

        img: function(rand) {
            var n = rand()*10|0, a = rand()<0.5, r = rand();
            return this.render([[n,a],
                r < 0.25 ? [n+1,a] :
                r < 0.50 ? [n+3,a] :
                r < 0.75 ? [n+3,!a] : [n+5,!a]]);
        },

        Cards: function(msg) {
            m.dom.clearCells();
            msg.cards.forEach((card, idx) => {
                m.dom.addCell(this.render(card), idx, 5);
            });
        }

    };

})();
