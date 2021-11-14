m.modal = (function() {

    var pwd = { password: localStorage.getItem('password') };

    var v = {};
    'newname'.split(' ').forEach(x => { v[x] = () => document.getElementById(x).value; });

    var actions = {

        namechange: () => {
            m.net.send('Uname', { uname: v.newname() });
            m.modal.close();
        },

        savestate: () => {
            m.net.send('SaveState', pwd);
        },

        creategame: () => {
            m.net.send('CreateGame', { gtype: v.gametype() });
        }

    };

    return {

        active: undefined,

        show: function(id) {
            if (this.active) return;
            this.active = document.getElementById('modal_'+id);
            this.active.style.display = 'flex';
        },

        close: function() {
            if (this.active) this.active.style.display = 'none';
            this.active = undefined;
        },

        _onload: function() {
            Array.from(document.getElementsByClassName('dismiss')).forEach(e => {
                e.addEventListener('click', this.close.bind(this));
            });
            for (a in actions) {
                document.getElementById(a).addEventListener('click', actions[a]);
            }
        }

    };

})();
