m.util = (function() {

    return {

        phi: 1.61803398875,

        range: function(n, f) {
            return Array(n).fill().map((_,i) => (f || (x=>x))(i));
        },

        shuffle: function(a, rand) {
            if (!rand) rand = Math.random;
            for (var i = a.length-1; i; --i) {
                var j = rand()*(i+1) | 0,
                    t = a[i];
                a[i] = a[j];
                a[j] = t;
            }
            return a;
        },

        // str->seed: http://www.cse.yorku.ca/~oz/hash.html
        // rand: https://stackoverflow.com/a/47593316/1223693
        srand: function(s) {
            var a = 5381;
            s.split('').forEach(c => a = a*33 + c.charCodeAt() | 0);
            return () => {
              var t = a += 0x6D2B79F5;
              t = Math.imul(t ^ t >>> 15, t | 1);
              t ^= t + Math.imul(t ^ t >>> 7, t | 61);
              return ((t ^ t >>> 14) >>> 0) / 4294967296;
            };
        }

    };

})();
