m.S3T2 = (function() {

    return {

        render: function(card) {
            var colors = ['#f00', '#ff0', '#09f', '#f80', '#0c0', '#90f'],
                x = i => [0,-1,1][i],
                y = (i,j) => [0,Math.sqrt(3),Math.sqrt(3)][i] + j*4,
                fill = m.conf.get('filled') ? '#888' : 'transparent';

            var svg = m.dom.svgel('svg', {
                _viewBox: '-2 -1 4 8'
            });

            m.util.range(2, i => {
                svg.appendChild(m.dom.svgel('path', {
                    d: `M ${x(0)} ${y(0,i)} L ${x(1)} ${y(1,i)} L ${x(2)} ${y(2,i)} Z`,
                    stroke: '#444', strokeWidth: 0.2, fill: (card[0]<3)^i ? fill : 'transparent'
                }));
            });

            card.forEach((p, i) => {
                // svg.appendChild(m.dom.svgel('circle', {
                //     cx: i/3|0, cy: i%3, r: 0.3,
                //     fill: colors[p], stroke: '#000', strokeWidth: 0.05
                // }));

                svg.appendChild(m.dom.svgel('circle', {
                    cx: x(i%3), cy: y(i%3, i/3|0),
                    r: 0.5,
                    fill: colors[p], stroke: '#000', strokeWidth: 0.05
                }));
            });

            return svg;
        },

        img: function(rand) {
            var a = m.util.shuffle([0,1,2], rand),
                b = m.util.shuffle([3,4,5], rand);
            return this.render(rand() < 0.5 ? a.concat(b) : b.concat(a));
        },

        conf: 'filled rownum square'.split(' '),

        Cards: function(msg) {
            m.dom.clearCells();
            msg.cards.forEach((card, idx) => {
                m.dom.addCell(this.render(card), idx, 3);
            });
        }

    };

})();
