m.net = (function() {

    return {

        send: function(t, obj) {
            this.ws.send(JSON.stringify({...obj, t: t}));
        },

        _onload: function() {
            this.ws = new WebSocket('ws://' + location.hostname + ':9255/');

            this.ws.onopen = () => {
                var userinfo = localStorage.getItem('userinfo');
                if (userinfo) this.send('Identify', JSON.parse(userinfo));
                else this.send('Register', { uname: prompt('enter a username') });
            };

            this.ws.onmessage = e => {
                var msg = JSON.parse(e.data);
                if (m.gm[msg.t]) m.gm[msg.t](msg);
                else if (m[m.game] && m[m.game][msg.t]) m[m.game][msg.t](msg);
                else console.log('bad msg type '+msg.t); // TODO
            };

            this.ws.onclose = () => {
                m.e.name.style.display = 'none';
                m.e.discon.style.display = 'block';
            };
        }

    };

})();
