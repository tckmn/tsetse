m.C3C3 = (function() {

    return {

        render: function(card) {
            var colors = [
                    ['#f00', '#ff0', '#09f'],
                    ['#f80', '#0c0', '#90f'],
                    ['#fff', '#aaa', '#3c3c3c']
                ],
                fillcolors = [
                    '#6fff0f', '#dd13bf', '#828282'
                ],
                x = i => Math.sin(2*Math.PI/3*i),
                y = i => Math.cos(2*Math.PI/3*i);

            var draw = m.draw.create('-3.5 -3.5 7 7');

            draw.el('circle', {
                cx: 0, cy: 0, r: 2,
                fill: 'transparent', stroke: '#000', strokeWidth: 0.3
            });

            card[1].forEach((t, i) => {
                draw.group({ transform: `translate(${2*x(i)}, ${2*y(i)})` });
                draw.poly([[x(0.5),y(0.5)],[x(1.5),y(1.5)],[x(2.5),y(2.5)]], {
                    fill: fillcolors[(i+card[0])%3], stroke: '#000', strokeWidth: 0.1
                });
                m.util.range(3, j => {
                    draw.el('circle', {
                        cx: x(j+0.5), cy: y(j+0.5), r: 0.4,
                        fill: colors[(i+card[0])%3][(j+t)%3],
                        stroke: '#000', strokeWidth: 0.05
                    });
                });
            });

            return draw.svg;
        },

        img: function(rand) {
            return this.render([rand()*3|0].concat([m.util.shuffle([0,1,2], rand)]));
        },

        conf: 'rownum square richter'.split(' '),

        ...m.setVariant.props(3)

    };

})();
