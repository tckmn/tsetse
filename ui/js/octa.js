m.OCTA = (function() {

    return {

        render: function(card) {
            var spirat = 0.3,
                op = r => i => [Math.sin(2*Math.PI/3*i)*r, Math.cos(2*Math.PI/3*i)*r],
                oout = op(1), oin = op(spirat),
                ocolor = [
                    '#9100d7',
                    '#ff9600',
                    '#ff00c8',
                    '#00be96'
                ];

            var draw = m.draw.create('-1 -1 2 2');

            // octahedron
            m.util.range(3, i => {
                draw.poly([oout(i), oout(i+0.5), oout(i+1)], {
                    fill: ocolor[card[0][i]], fancy: 1
                });
            });
            draw.poly(m.util.range(3, i => oout(i)), {
                fill: ocolor[card[0][3]], fancy: 1
            });
            draw.fancy(0.16, 0.1);

            // parity swirl
            m.util.range(3, i => {
                draw.el('path', {
                    d: `M 0 0 A ${spirat/2} ${spirat/2} 0 0 ${card[1] ? 0 : 1} ${oin(i).join(' ')}`,
                    fill: 'none', stroke: card[1] ? '#000' : '#fff', strokeWidth: 0.05,
                    strokeLinejoin: 'round', strokeLinecap: 'round'
                });
            });

            return draw.svg;
        },

        img: function(rand) {
            return this.render([m.util.shuffle(m.util.range(4), rand), rand() < 0.5]);
        },

        conf: 'rownum square'.split(' '),

        Cards: function(msg) {
            m.dom.clearCells();
            msg.cards.forEach((card, idx) => {
                m.dom.addCell(this.render(card), idx, 3);
            });
        }

    };

})();
