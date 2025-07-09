m.net = (function() {

    var prevmsg = {};

    var recv = msg => {
        prevmsg[msg.t] = msg;
        if (m.gm[msg.t]) m.gm[msg.t](msg);
        else if (m[m.game] && m[m.game][msg.t]) m[m.game][msg.t](msg);
        else console.log('bad msg type '+msg.t); // TODO
    };

    var backoff = 1;

    return {

        ws: undefined,

        connect: function() {
            m.e.name.textContent = 'connecting...';
            m.e.name.style.display = 'block';
            m.e.discon.style.display = 'none';

            this.ws = new WebSocket(localStorage.ws || 'wss://' + location.hostname + '/ws/');

            this.ws.onopen = () => {
                backoff = 1;
                var userinfo = localStorage.getItem('userinfo');
                if (userinfo) {
                    userinfo = JSON.parse(userinfo);
                    m.cid = userinfo.cid;
                    this.send('Identify', userinfo);
                } else this.register();
            };

            this.ws.onmessage = e => {
                recv(JSON.parse(e.data));
            };

            this.ws.onclose = () => {
                m.e.name.style.display = 'none';
                m.e.discon.style.display = 'block';
                setTimeout(this.connect.bind(this), backoff*1000);
                backoff = (1 + Math.exp(-backoff/100))*backoff;
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
