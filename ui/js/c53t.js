m.C53T = (function() {

    return {

        Cards: function(msg) {

            m.dom.clr(m.e.wall);
            m.dom.cells = [];

            var five = [0,1,2,3,4],
                colors = ['#f00', '#90f', '#00f', '#0d0', '#f09600'],
                xsep = m.conf.get('offset') ? 1 : 0, ysep = 2.5,
                pentW = 0.2, lineW = 0.2, outlineW = 0.06;

            msg.cards.forEach((card, idx) => {

                var svg = m.dom.svgel('svg', {
                    _viewBox: `-${xsep+1.5} -${ysep/2+0.2} ${xsep*2+3} ${ysep*3+0.2}`
                });

                // svg.appendChild(svgel('rect', {x:-1.5,y:-sep/2,width:3,height:sep*3,fill:'red'}));

                card.forEach((p, i) => {
                    var cx = i*xsep-xsep,
                        cy = i*ysep,
                        x = j => cx + Math.sin(Math.PI*2/5*j),
                        y = j => cy - Math.cos(Math.PI*2/5*j),
                        stroke = ['#aaa','#666','#000'][i],
                        fill = m.conf.get('filled') ? ['#ddd', '#999', '#333'][i] : 'transparent';

                    // the pentagon
                    svg.appendChild(m.dom.svgel('path', {
                        d: five.map(j => `${j?'L':'M'} ${x(j)} ${y(j)}`).join(' ') + 'Z',
                        stroke: stroke, strokeWidth: pentW, fill: fill
                    }));

                    // outline of line from center to point
                    svg.appendChild(m.dom.svgel('path', {
                        d: `M ${cx} ${cy} L ${x(p)} ${y(p)}`,
                        stroke: stroke, strokeWidth: lineW + 2*outlineW
                    }));

                    // circles at points
                    five.forEach(j => {
                        svg.appendChild(m.dom.svgel('circle', {
                            cx: x(j), cy: y(j), r: 0.2,
                            stroke: stroke, strokeWidth: outlineW, fill: colors[j]
                        }));
                    });

                    // circle at center
                    svg.appendChild(m.dom.svgel('circle', {
                        cx: cx, cy: cy, r: 0.2,
                        stroke: stroke, strokeWidth: outlineW, fill: colors[p]
                    }));

                    // line from center to point
                    svg.appendChild(m.dom.svgel('path', {
                        d: `M ${cx} ${cy} L ${x(p)} ${y(p)}`,
                        stroke: colors[p], strokeWidth: lineW
                    }));
                });

                m.dom.addCell(svg, idx);

            });

        },

        Highlight: msg => {

            var kls = msg.good ? 'right' : 'wrong';
            m.dom.cells.forEach(cell => {
                if (msg.idxs.indexOf(+cell.dataset.idx) !== -1) cell.classList.add(kls);
            });
            if (!msg.good) setTimeout(() => {
                m.dom.cells.forEach(cell => cell.classList.remove(kls));
            }, 200);

        }

    };

})();
