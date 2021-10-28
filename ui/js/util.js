m.util = (function() {

    return {

        range: function(n, f) {
            return Array(n).fill().map((_,i) => f(i));
        }

    };

})();
