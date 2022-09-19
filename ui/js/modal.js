m.modal = (function() {

    var pwd = (o, k) => {
        var p = localStorage.getItem('password');
        if (p) o[k || 'password'] = p;
        return o;
    };

    var v = {};

    var actions = {

        namechange: () => {
            m.net.send('Uname', { uname: v.newname() });
            m.modal.close();
        },

        savestate: () => {
            m.net.send('SaveState', pwd({}));
            m.modal.close();
        },

        exhibitset: () => {
            m.net.send('ExhibitSet', pwd({}));
            m.modal.close();
        },

        creategame: () => {
            var g = document.getElementById('gtchoice');
            if (!g) {
                m.dom.toast('choose a game!');
                return;
            }
            var conf;
            try {
                conf = JSON.parse(v.gameconfig());
            } catch (e) {
                m.dom.toast("your config is not valid json");
                return;
            }
            m.net.send('CreateGame', { gtype: g.dataset.g, conf: conf });
            m.modal.close();
        },

        delgame: () => {
            m.net.send('DeleteGame', pwd({ gid: info }, 'mpassword'));
            m.modal.close();
        }

    };

    var active = undefined, info = undefined;

    return {

        show: function(id, info2) {
            if (active) return;
            active = document.getElementById('modal_'+id);
            active.style.display = 'flex';
            info = info2;
        },

        close: function() {
            if (active) active.style.display = 'none';
            active = undefined;
        },

        _onload: function() {
            Array.from(document.getElementsByClassName('mval')).forEach(x => {
                v[x.dataset.mval] = () => x.value;
            });
            Array.from(document.getElementsByClassName('mopen')).forEach(x => {
                x.addEventListener('click', () => this.show(x.dataset.mopen));
            });
            Array.from(document.getElementsByClassName('dismiss')).forEach(e => {
                e.addEventListener('click', this.close.bind(this));
            });
            for (a in actions) {
                document.getElementById(a).addEventListener('click', actions[a]);
            }
        }

    };

})();
