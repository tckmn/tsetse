m.net = (function() {

    var prevmsg = {};

    var recv = msg => {
        prevmsg[msg.t] = msg;
        if (m.gm[msg.t]) m.gm[msg.t](msg);
        else if (m[m.game] && m[m.game][msg.t]) m[m.game][msg.t](msg);
        else console.log('bad msg type '+msg.t); // TODO
    };

    return {

        send: function(t, obj) {
            this.ws.send(JSON.stringify({...obj, t: t}));
        },

        rerun: function(t) {
            if (prevmsg[t]) recv(prevmsg[t]);
        },

        register: function() {
            this.send('Register', { uname: prompt('enter a username') });
        },

        _onload: function() {
            this.ws = new WebSocket('ws://' + location.hostname + ':9255/');

            this.ws.onopen = () => {
                var userinfo = localStorage.getItem('userinfo');
                if (userinfo) this.send('Identify', JSON.parse(userinfo));
                else this.register();
            };

            this.ws.onmessage = e => {
                recv(JSON.parse(e.data));
            }

            this.ws.onclose = () => {
                m.e.name.style.display = 'none';
                m.e.discon.style.display = 'block';
            };
        }

    };

})();
