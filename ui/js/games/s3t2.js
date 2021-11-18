m.S3CT = (function() {

    return {

        render: function(card) {
            var colors = ['#f00', '#ff0', '#09f', '#f80', '#0c0', '#90f'],
                x = i => Math.sin(2*Math.PI/3*i),
                y = i => Math.cos(2*Math.PI/3*i),
                fill = m.conf.get('filled') ? '#888' : 'transparent';

            var draw = m.draw.create('-2 -3.5 4 7');

            m.util.range(2, i => {
                // svg.appendChild(m.dom.svgel('path', {
                //     d: `M ${x(0)} ${y(0,i)} L ${x(1)} ${y(1,i)} L ${x(2)} ${y(2,i)} Z`,
                //     stroke: '#444', strokeWidth: 0.2, fill: (card[0]<3)^i ? fill : 'transparent'
                // }));
            });

            card.forEach((p, i) => {
                // svg.appendChild(m.dom.svgel('circle', {
                //     cx: i/3|0, cy: i%3, r: 0.3,
                //     fill: colors[p], stroke: '#000', strokeWidth: 0.05
                // }));

                if (!(i%3)) {
                    var parity = ((card[i+0]>card[i+1])+(card[i+0]>card[i+2])+(card[i+1]>card[i+2]))%2;
                    draw.group({ transform: `rotate(180) translate(0, ${(i-1.5)*.9}) rotate(${i?0:60})` });
                    // if (p < 3) draw.el('circle', {
                    //     r: 1.2, fill: '#aaa'
                    // });
                    draw.fidget(3, 1, parity,
                        {
                            stroke: p < 3 ? '#000' : '#aaa', strokeWidth: 0.1,
                            offset: (parity ? -1 : 1) * 0.15,
                            // strokeDasharray: parity ? '0.1 0.2' : 'none'
                        });
                }

                draw.el('circle', {
                    cx: x(i%3), cy: y(i%3),
                    r: 0.4,
                    fill: colors[p], stroke: '#000', strokeWidth: 0.05
                });
            });

            return draw.svg;
        },

        img: function(rand) {
            var a = m.util.shuffle([0,1,2], rand),
                b = m.util.shuffle([3,4,5], rand);
            return this.render(rand() < 0.5 ? a.concat(b) : b.concat(a));
        },

        ...m.setVariant.props({ boardSize: 10, autosubmit: 3 })

    };

})();
