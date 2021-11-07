m.net = (function() {

    var prevmsg = {};

    var recv = msg => {
        prevmsg[msg.t] = msg;
        if (m.gm[msg.t]) m.gm[msg.t](msg);
        else if (m[m.game] && m[m.game][msg.t]) m[m.game][msg.t](msg);
        else console.log('bad msg type '+msg.t); // TODO
    };

    return {

        ws: undefined,
        backoff: 1,

        connect: function() {
            m.e.name.textContent = 'connecting...';
            m.e.name.style.display = 'block';
            m.e.discon.style.display = 'none';

            this.ws = new WebSocket('wss://' + location.hostname + '/ws/');

            this.ws.onopen = () => {
                this.backoff = 1;
                var userinfo = localStorage.getItem('userinfo');
                if (userinfo) this.send('Identify', JSON.parse(userinfo));
                else this.register();
            };

            this.ws.onmessage = e => {
                recv(JSON.parse(e.data));
            };

            this.ws.onclose = () => {
                m.e.name.style.display = 'none';
                m.e.discon.style.display = 'block';
                setTimeout(this.connect.bind(this), this.backoff*1000);
                this.backoff = (1 + Math.exp(-this.backoff/100))*this.backoff;
            };
        },

        send: function(t, obj) {
            this.ws.send(JSON.stringify({...obj, t: t}));
        },

        rerun: function(t) {
            if (prevmsg[t]) recv(prevmsg[t]);
        },

        register: function() {
            var uname = prompt('enter a username');
            if (uname === null) {
                m.e.name.style.display = 'none';
                m.e.discon.style.display = 'block';
                m.e.discon.textContent = 'you must provide a username';
                return;
            }
            this.send('Register', { uname: uname });
        },

        _onload: function() {
            this.connect();
        }

    };

})();
