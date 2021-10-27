m.conf = (function() {

    var spec = {
        offset: {
            default: false
        },
        filled: {
            default: false
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
        'c53t': 'offset filled rownum square'.split(' ')
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
                var obj = spec[key];

                var cont  = document.getElementById('conf'+key),
                    box   = cont.getElementsByClassName('box')[0],
                    minus = cont.getElementsByClassName('minus')[0],
                    plus  = cont.getElementsByClassName('plus')[0],
                    disp  = cont.getElementsByClassName('disp')[0],
                    updfn = (v, nowrite) => {
                        settings[m.game][key] = v;
                        if (obj.update) obj.update(v);
                        if (box) box.checked = v;
                        if (disp) disp.textContent = v;
                        if (!nowrite) localStorage.setItem('settings', JSON.stringify(settings));
                    };

                cont.style.display = 'block';
                updfn(this.get(key), true);

                if (minus) minus.addEventListener('click', () => updfn(Math.max(obj.min, settings[m.game][key]-1)));
                if (plus) plus.addEventListener('click', () => updfn(Math.min(obj.max, settings[m.game][key]+1)));
                if (box) box.addEventListener('change', () => updfn(box.checked));
            });
        }

    };

})();
