m.FOLD = (function() {

    return {

        render: function(card) {
            var draw = m.draw.create('-1 -1 2 2', { class: 'grow' });

            var offset = 0.5,
                p = (m,j) => [2*Math.sin(Math.PI*2/6*j), 2*Math.cos(Math.PI*2/6*j) + m*offset];

            draw.line([[0, -offset], p(-1, card[0][0])], {
                stroke: card[0][1] ? '#f00' : '#00f', strokeWidth: 0.05
            });
            draw.line([[0, offset], p(1, card[1][0]+3)], {
                stroke: card[1][1] ? '#f00' : '#00f', strokeWidth: 0.05
            });
            draw.line([[0, -offset], [0, offset]], {
                stroke: '#000', strokeWidth: 0.05
            });

            return draw.svg;
        },

        img: function(rand) {
            return this.render([[rand()*6|0, rand()<0.5], [rand()*6|0, rand()<0.5]]);
        },

        ...m.setVariant.props({ autosubmit: 5 })

    };

})();
