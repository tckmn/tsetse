m.modal = (function() {

    var pwd = { password: localStorage.getItem('password') };

    var v = {};

    var actions = {

        namechange: () => {
            m.net.send('Uname', { uname: v.newname() });
            m.modal.close();
        },

        savestate: () => {
            m.net.send('SaveState', pwd);
            m.modal.close();
        },

        exhibitset: () => {
            m.net.send('ExhibitSet', pwd);
            m.modal.close();
        },

        creategame: () => {
            var conf;
            try {
                conf = JSON.parse(v.gameconfig());
            } catch (e) {
                m.dom.toast("your config is not valid json");
                return;
            }
            m.net.send('CreateGame', { gtype: v.gametype(), conf: conf });
            m.modal.close();
        }

    };

    var active = undefined;

    return {

        show: function(id) {
            if (active) return;
            active = document.getElementById('modal_'+id);
            active.style.display = 'flex';
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
