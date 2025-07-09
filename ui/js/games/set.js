m.SET = (function() {

    return {

        render: function(card) {
            var ysep = 2.5,
                shapeW = 2, shadeW = 0.3, outlineW = 0.2,
                squigW = 1.7, squigL = 0.5, squigH = 1.0, squigC = 0.8, squigA = 0.6;

            var color = ['#f00', '#90f', '#0d0'][card[0]],
                fill = ['transparent', color, color][card[1]],
                mask = [undefined, 'url(#shademask)', undefined][card[1]],
                shape = [
                    `M 0 -1 L ${shapeW} 0 L 0 1 L -${shapeW} 0 Z`,
                    `M 0 -1 L 1 -1 Q ${shapeW} -1 ${shapeW} 0 Q ${shapeW} 1 1 1 L -1 1 Q -${shapeW} 1 -${shapeW} 0 Q -${shapeW} -1 -1 -1 Z`,
                    `
                    M -${squigW} 1
                    C -${squigW+squigC} 1 -${squigH+squigC} -1 -${squigH} -1
                    C 0 -1 0 -${squigA} ${squigL} -${squigA}
                    C ${squigL+squigC} -${squigA} ${squigW-squigC} -1 ${squigW} -1
                    C ${squigW+squigC} -1 ${squigH+squigC} 1 ${squigH} 1
                    C 0 1 0 ${squigA} -${squigL} ${squigA}
                    C -${squigL+squigC} ${squigA} -${squigW-squigC} 1 -${squigW} 1
                    Z`
                ][card[2]];

            var svg = m.dom.svgel('svg', {
                _viewBox: `-${shapeW+outlineW} -${ysep/2+0.2} ${shapeW*2+outlineW*2} ${ysep*3+0.4}`
            });

            // svg.appendChild(m.dom.svgel('rect', {x:-(shapeW+outlineW),y:-(ysep/2+0.2),width:shapeW*2+outlineW*2,height:ysep*3+0.4,fill:'red'}));

            svg.appendChild(m.dom.svgel('pattern', {
                id: 'shadepat',
                width: shadeW, height: shadeW, _patternUnits: 'userSpaceOnUse',
                children: [m.dom.svgel('rect', {
                    width: shadeW/3, height: shadeW, fill: 'white'
                })]
            }));
            svg.appendChild(m.dom.svgel('mask', {
                id: 'shademask',
                children: [m.dom.svgel('rect', {
                    x: -99, y: -99, width: 999, height: 999, fill: 'url(#shadepat)'
                })]
            }));

            for (let i = 0; i <= card[3]; ++i) {
                var cy = i*ysep+0.5*ysep*(2-card[3]);

                svg.appendChild(m.dom.svgel('path', {
                    transform: `translate(0 ${cy})`,
                    d: shape, fill: fill, mask: mask
                }));
                svg.appendChild(m.dom.svgel('path', {
                    transform: `translate(0 ${cy})`,
                    d: shape, stroke: color, strokeWidth: outlineW, fill: 'transparent'
                }));
            }

            return svg;
        },

        img: function(rand) {
            return this.render([rand()*3|0, rand()*3|0, rand()*3|0, rand()*3|0]);
        },

        ...m.setVariant.props({
            autosubmit: 3
        })

    };

})();
