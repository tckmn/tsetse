m.SAT = (function() {

    return {

        render: function(card) {
            var colors = ['#f00', '#ff0', '#09f', '#f80', '#0c0', '#90f', '#000', '#888'],
                xsize = 0.4;

            var draw = m.draw.create('-1 -1 4 4');

            card.forEach(v => {
                var vv = Math.abs(v)-1, x = vv / 3 | 0, y = vv % 3;
                draw.el('circle', {
                    cx: x, cy: y, r: 0.4,
                    fill: colors[vv],
                    stroke: '#000', strokeWidth: 0.05
                });
                if (v < 0) {
                    draw.el('path', {
                        d: `M ${x-xsize} ${y-xsize} l ${2*xsize} ${2*xsize} M ${x-xsize} ${y+xsize} l ${2*xsize} ${-2*xsize}`,
                        stroke: '#000', strokeWidth: 0.1
                    });
                }
            });

            return draw.svg;
        },

        img: function(rand) {
            return this.render(new Array(6).fill().map(_ => rand() < 0.5));
        },

        ...m.setVariant.props()

    };

})();
