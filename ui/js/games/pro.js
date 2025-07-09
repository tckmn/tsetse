m.PRO = (function() {

    return {

        render: function(card, conf) {
            var colors = ['#f00', '#ff0', '#09f', '#f80', '#0c0', '#90f', '#000', '#888'],
                xsize = 0.4;

            var draw = m.draw.create(`-1 -1 3 4`);

            card.forEach((v,i) => {
                var x = i / 3 | 0, y = i % 3;
                if (v) draw.el('circle', {
                    cx: x, cy: y, r: 0.4,
                    fill: colors[i],
                    stroke: '#000', strokeWidth: 0.05
                });
            });

            return draw.svg;
        },

        img: function(rand, conf) {
            return this.render(m.util.range(6, _=>rand()<0.5), conf);
        },

        ...m.setVariant.props({
            boardSize: 7
        })

    };

})();
