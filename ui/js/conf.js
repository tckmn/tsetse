m.conf = (function() {

    var richterval, rots = Array(50).fill(0);

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
            init: n => m.e.wall.style.gridTemplateColumns = `repeat(${n},1fr)`,
            update: () => m.net.rerun('Cards') // for hotkeys
        },
        square: {
            default: true,
            init: () => m.dom.resize()
        },
        richter: {
            default: 0, min: 0, max: 9,
            init: n => {
                clearInterval(richterval);
                if (!n) {
                    rots = Array(50).fill(0);
                    m.dom.cells.forEach(c => c.style.transform = '');
                    return;
                }
                richterval = setInterval(() => {
                    m.dom.cells.forEach((c,i) => {
                        c.style.transform = `rotate(${rots[i]+=(Math.random()-0.5)*Math.pow(1.5, n)}deg)`;
                    });
                }, 50);
            },
            deinit: () => {
                clearInterval(richterval);
            }
        }
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
            if (m[m.game]) (m[m.game].conf || []).forEach(key => {
                spec[key].updfn(this.get(key), true);
                document.getElementById('conf'+key).style.display = 'block';
            });
        },

        deinit: function() {
            if (m[m.game]) (m[m.game].conf || []).forEach(key => {
                if (spec[key].deinit) spec[key].deinit(this.get(key));
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

                obj.updfn = (v, isinit) => {
                    settings[m.game][key] = v;
                    if (box) box.checked = v;
                    if (disp) disp.textContent = v;
                    if (obj.init) obj.init(v);
                    if (!isinit) {
                        if (obj.update) obj.update(v);
                        localStorage.setItem('settings', JSON.stringify(settings));
                    }
                };

                if (minus) minus.addEventListener('click', () => obj.updfn(Math.max(obj.min, settings[m.game][key]-1)));
                if (plus) plus.addEventListener('click', () => obj.updfn(Math.min(obj.max, settings[m.game][key]+1)));
                if (box) box.addEventListener('change', () => obj.updfn(box.checked));
            })(key); }
        }

    };

})();
