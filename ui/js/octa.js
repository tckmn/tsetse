m.OCTA = (function() {

    return {

        render: function(card) {
            var op = i => [Math.sin(2*Math.PI/3*i), Math.cos(2*Math.PI/3*i)],
                ocolor = [
                    '#9100d7',
                    '#ff9600',
                    '#ff00c8',
                    '#00be96'
                ];

            var draw = m.draw.create('-1 -1 2 2');

            m.util.range(3, i => {
                draw.poly([op(i), op(i+0.5), op(i+1)], {
                    fill: ocolor[card[0][i]], fancy: 1
                });
            });
            draw.poly(m.util.range(3, i => op(i)), {
                fill: ocolor[card[0][3]], fancy: 1
            });

            draw.fancy(0.16, 0.1);

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
