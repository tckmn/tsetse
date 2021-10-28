m.conf = (function() {

    var spec = {
        offset: {
            default: false,
            update: () => m.net.rerun('Cards')
        },
        filled: {
            default: false,
            update: () => m.net.rerun('Cards')
        },
        rownum: {
            default: 4, min: 1, max: 12,
            update: n => m.e.wall.style.gridTemplateColumns = `repeat(${n},1fr)`
        },
        square: {
            default: true,
            update: () => m.dom.resize()
        }
    };

    var gameconf = {
        '': [],
        'C53T': 'offset filled rownum square'.split(' '),
        'FO1D': 'rownum square'.split(' ')
    };

    var settings = localStorage.getItem('settings');
    settings = settings ? JSON.parse(settings) : {};

    return {

        get: function(s) {
            return settings[m.game] && settings[m.game][s] !== undefined ?
                settings[m.game][s] : spec[s].default;
        },

        init: function() {
            Array.from(m.e.sbconf.children).forEach(c => c.style.display = 'none');
            if (!settings[m.game]) settings[m.game] = {};
            gameconf[m.game].forEach(key => {
                spec[key].updfn(this.get(key), true);
                document.getElementById('conf'+key).style.display = 'block';
            });
        },

        _onload: function() {
            for (key in spec) { (key => {
                var obj = spec[key],
                    cont  = document.getElementById('conf'+key),
                    box   = cont.getElementsByClassName('box')[0],
                    minus = cont.getElementsByClassName('minus')[0],
                    plus  = cont.getElementsByClassName('plus')[0],
                    disp  = cont.getElementsByClassName('disp')[0];

                obj.updfn = (v, nowrite) => {
                    settings[m.game][key] = v;
                    if (obj.update) obj.update(v);
                    if (box) box.checked = v;
                    if (disp) disp.textContent = v;
                    if (!nowrite) localStorage.setItem('settings', JSON.stringify(settings));
                };

                if (minus) minus.addEventListener('click', () => obj.updfn(Math.max(obj.min, settings[m.game][key]-1)));
                if (plus) plus.addEventListener('click', () => obj.updfn(Math.min(obj.max, settings[m.game][key]+1)));
                if (box) box.addEventListener('change', () => obj.updfn(box.checked));
            })(key); }
        }

    };

})();
