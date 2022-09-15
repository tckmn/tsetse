m.OC = (function() {

    return {

        img: function(rand) {

            var draw = m.draw.create('0 0 1 1');
            draw.el('text', {
                text: 'OC',
                x: 0.5, y: 0.5, fontSize: 0.5,
                textAnchor: 'middle',
                dominantBaseline: 'middle',
            });
            return draw.svg;

        },

        defaultConfig: {},

        conf: {}

    };

})();
