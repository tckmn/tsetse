m.conf = (function() {

    var richterval, rots = {};

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
            init: () => setTimeout(m.dom.resize, 0) // because css needs to process
        },
        richter: {
            default: 0, min: 0, max: 9,
            init: n => {
                clearInterval(richterval);
                if (!n) {
                    m.dom.eachCell(c => c.style.transform = '');
                    return;
                }
                richterval = setInterval(() => {
                    m.dom.eachCell((c,i) => {
                        c.style.transform = `rotate(${rots[i]=(rots[i]||0)+(Math.random()-0.5)*Math.pow(1.5, n)}deg)`;
                    });
                }, 50);
            },
            deinit: () => {
                clearInterval(richterval);
            }
        }
    };

    var gspec = (k,q) =>
           m[m.game]
        && m[m.game].conf
        && m[m.game].conf[k]
        && m[m.game].conf[k][q] !== undefined ?
        m[m.game].conf[k][q] : spec[k][q];

    var gcall = (k,q,...args) => {
        var fn = gspec(k, q);
        if (fn) return fn(...args);
    };

    var settings = localStorage.getItem('settings');
    settings = settings ? JSON.parse(settings) : {};

    return {

        get: function(s) {
            return settings[m.game] && settings[m.game][s] !== undefined ?
                settings[m.game][s] : gspec(s, 'default');
        },

        init: function() {
            Array.from(m.e.sbconf.children).forEach(c => c.style.display = 'none');
            if (!settings[m.game]) settings[m.game] = {};
            if (m[m.game]) Object.keys(m[m.game].conf || {}).forEach(key => {
                spec[key].updfn(this.get(key), true);
                document.getElementById('conf'+key).style.display = 'block';
            });
        },

        deinit: function() {
            if (m[m.game]) Object.keys(m[m.game].conf || {}).forEach(key => {
                gcall(key, 'deinit', this.get(key));
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
                    gcall(key, 'init', v);
                    if (!isinit) {
                        gcall(key, 'update', v);
                        localStorage.setItem('settings', JSON.stringify(settings));
                    }
                };

                if (minus) minus.addEventListener('click', () => obj.updfn(Math.max(gspec(key, 'min'), settings[m.game][key]-1)));
                if (plus) plus.addEventListener('click', () => obj.updfn(Math.min(gspec(key, 'max'), settings[m.game][key]+1)));
                if (box) box.addEventListener('change', () => obj.updfn(box.checked));
            })(key); }
        }

    };

})();
