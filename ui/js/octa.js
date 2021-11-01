m.OCTA = (function() {

    return {

        render: function(card) {
            var svg = m.dom.svgel('svg', {
                _viewBox: '-2 -2 4 4'
            });

            return svg;
        },

        img: function(rand) {
            return this.render([rand()*5|0, rand()*5|0, rand()*5|0]);
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
