m.OCTA = (function() {

    return {

        render: function(card) {
            var pad = 0.2,
                spirat = 0.3,
                op = r => i => [Math.sin(2*Math.PI/3*i)*r, Math.cos(2*Math.PI/3*i)*r],
                oout = op(1), oin = op(spirat),
                ocolor = [
                    '#9100d7',
                    '#ff9600',
                    '#ff00c8',
                    '#00be96'
                ],
                cfaces = [
                    [1,2,3,0],
                    [3,2,0,1],
                    [1,3,0,2]
                ],
                ccolor = [
                    '#f00',
                    '#0f0',
                    '#00f'
                ];

            var draw = m.draw.create(`-${1+pad} -${2+1.5*pad} ${2+2*pad} ${4+3*pad}`);

            // cube
            draw.group({ transform: `rotate(180) translate(0,-${1+pad/2})` });
            var idx = card[0].indexOf(0);
            cfaces.forEach((c, i) => {
                var quad = [[0,0], oout(i-0.5), oout(i), oout(i+0.5)],
                    a = card[0][c[idx]],
                    b = card[0][c[c[idx]]];
                draw.poly(quad, {
                    fill: ccolor[b-1], fancy: 1
                });
                if (((b-a+3)%3 === 1) ^ card[1]) {
                    draw.poly(quad, {
                        fill: '#fff',
                        transform: `scale(0.6) translate(${op(0.35)(i).join(',')})`
                    });
                }
            });
            draw.fancy(0.08, 0.05);

            // octahedron
            draw.group({ transform: `rotate(180) translate(0,${1+pad/2})` });
            m.util.range(3, i => {
                draw.poly([oout(i), oout(i+0.5), oout(i+1)], {
                    fill: ocolor[card[0][i]], fancy: 1
                });
            });
            draw.poly(m.util.range(3, i => oout(i)), {
                fill: ocolor[card[0][3]], fancy: 1
            });
            draw.fancy(0.08, 0.05);
            draw.fidget(3, spirat, card[1], {
                stroke: card[1] ? '#000' : '#fff', strokeWidth: 0.05
            });

            return draw.svg;
        },

        img: function(rand) {
            return this.render([m.util.shuffle(m.util.range(4), rand), rand() < 0.5]);
        },

        conf: 'rownum square'.split(' '),

        ...m.setVariant.props(3)

    };

})();
