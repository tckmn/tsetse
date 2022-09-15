m.A5SET = (function() {

    return {

        render: function(card) {
            // https://github.com/redstonerodent/set-variants/blob/7f2218ed0044c90003259ad4a29826f1fc1f38e7/dodecahedron.py
            var pad = 0.2;

            var dp = r => t => [Math.sin(2*Math.PI/5*t)*r, Math.cos(2*Math.PI/5*t)*r],
                din = dp(1/m.util.phi), dout = dp(1),
                dfaces = [
                    [m.util.range(5, i => din(i)), m.util.range(5)],
                    ...m.util.range(5, i => [
                        [din(i), din(i+1), dout(i+1), dout(i+0.5), dout(i)],
                        m.util.range(5, j => (i+[0,2,1,4,3][(j-i+5)%5])%5)
                    ])
                ],
                dcolor = p => ({
                     6: '#ff1e1e',
                    11: '#ffdc00',
                    14: '#00e13c',
                    19: '#00bcdc',
                    21: '#8032ff',
                    24: '#d732ff'
                })[Math.pow((p[1]-p[0]+5)%5 + 4*((p[2]-p[0]+5)%5), 2) % 25],
                ifaces = [
                    ...m.util.range(5, i => [
                        [[0,0], dout(i-1.5), dout(i-0.5)], i
                    ]),
                    ...m.util.range(5, i => [
                        [dout(i+0.5), dout(i+1), dout(i+1.5)], i
                    ])
                ],
                icolor = [
                    '#ff8000',
                    '#1c9100',
                    '#7300af',
                    '#3232ff',
                    '#eb00b9'
                ];

            var draw = m.draw.create(`-${1+pad} -${2+1.5*pad} ${2+2*pad} ${4+3*pad}`);

            draw.group({ transform: `translate(0,-${1+pad/2})` });
            dfaces.forEach(([pent, perm]) => {
                draw.poly(pent, {
                    fill: dcolor(m.util.range(5, i => perm[card[i]])), fancy: 1
                });
            });
            draw.fancy(0.1, 0.05);

            draw.group({ transform: `translate(0,${1+pad/2})` });
            ifaces.forEach(([tri, i]) => {
                draw.poly(tri, {
                    fill: icolor[card.indexOf(i)], fancy: 1
                });
            });
            draw.fancy(0.1, 0.05);

            return draw.svg;
        },

        img: function(rand) {
            var a = m.util.shuffle(m.util.range(5), rand), sign = true;
            m.util.range(5, i => m.util.range(5, j => sign += j>i && a[i]>a[j]));
            if (sign) {
                var t = a[0];
                a[0] = a[1];
                a[1] = t;
            }
            return this.render(a);
        },

        ...m.setVariant.props({ boardSize: 10, autosubmit: 3 })

    };

})();
