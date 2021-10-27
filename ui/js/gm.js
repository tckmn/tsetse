m.gm = (function() {

    return {

        Registered: function(msg) {
            localStorage.setItem('userinfo', JSON.stringify({
                cid: msg.cid,
                secret: msg.secret
            }));
            m.e.name.textContent = msg.name;
        },

        Identified: function(msg) {
            m.e.name.textContent = msg.name;
        },

        NotIdentified: function(msg) {
            localStorage.clear();
            location.reload();
        },

        UserList: function(msg) {

            m.dom.clr(m.e.sbmain);

            var tbl = m.dom.el('table', { 'class': 'userlist' });

            msg.list
                .sort((a,b) => (b.score - (b.conn?0:999)) - (a.score - (a.conn?0:999)))
                .forEach(u => {
                    var tr = m.dom.el('tr');
                    if (u.conn) tr.classList.add('conn');
                    if (u.play) tr.classList.add('play');
                    tr.appendChild(m.dom.el('td', { text: u.score }));
                    tr.appendChild(m.dom.el('td', { text: u.name }));
                    tbl.appendChild(tr);
                });

            m.e.sbmain.appendChild(tbl);

        },

        GameList: function(msg) {

            m.dom.clr(m.e.wall);
            m.dom.cells = [];

            msg.list.forEach(g => {
                m.e.wall.appendChild(m.dom.el('a', {
                    text: g[1], href: '#',
                    onclick: e => {
                        e.preventDefault();
                        m.net.send('JoinGame', { gid: g[0] });
                    }
                }));
            });

            var sel = m.dom.el('select');
            'c53t'.split(' ').forEach(g => {
                sel.appendChild(m.dom.el('option', {
                    text: g, value: g
                }));
            });

            var newsec = m.dom.el('div');
            newsec.appendChild(sel);
            newsec.appendChild(m.dom.el('button', {
                text: 'create game',
                onclick: () => {
                    m.net.send('CreateGame', { gtype: sel.value });
                }
            }));

            m.e.wall.appendChild(newsec);

        },

        GameType: function(msg) {
            m.dom.clr(m.e.wall);
            m.dom.clr(m.e.sbmain);
            m.dom.cells = [];
            m.conf.init(m.game = msg.gtype);
        }

    };

})();
