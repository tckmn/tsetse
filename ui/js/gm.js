m.gm = (function() {

    return {

        Registered: function(msg) {
            localStorage.setItem('userinfo', JSON.stringify({
                cid: msg.cid,
                secret: msg.secret
            }));
            m.e.name.textContent = msg.name;
        },

        NotRegistered: function(msg) {
            m.net.register();
        },

        Identified: function(msg) {
            m.e.name.textContent = msg.name;
        },

        NotIdentified: function(msg) {
            localStorage.removeItem('userinfo');
            m.net.register();
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

            m.dom.clr(m.e.gamelist);

            var sel = m.dom.el('select');
            'C53T FOLD FO1D OCTA A5SET'.split(' ').forEach(g => {
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

            m.e.gamelist.appendChild(newsec);

            msg.list.forEach(g => {
                var gamerect = m.dom.el('div', {
                    class: 'gamerect',
                    onclick: () => {
                        m.net.send('JoinGame', { gid: g[0] });
                    }
                });
                gamerect.appendChild(m.dom.el('span', { text: g[1][0], class: 'gamename' }));
                gamerect.appendChild(m.dom.el('span', { text: g[1][1], class: 'gamedesc' }));
                gamerect.appendChild(m.dom.el('span', { text: `by ${g[1][2]} at ${new Date(g[1][3]).toLocaleString()}`, class: 'gamesrc' }));
                m.e.gamelist.appendChild(gamerect);
            });

        },

        GameType: function(msg) {
            m.dom.clr(m.e.wall);
            m.dom.clr(m.e.sbmain);
            m.dom.cells = [];
            m.conf.init(m.game = msg.gtype);
            // bit of an ugly hack to put this here
            m.e.wrap.classList.toggle('hasgame', !!m.game);
            m.e.helplink.setAttribute('target', m.game ? '_blank' : '_self');
            m.e.helplink.setAttribute('href', '/help.html' + (m.game ? '#'+m.game : ''));
            m.e.helplink.textContent = m.game ? 'rules' : 'help / about';
        },

        Highlight: function(msg) {

            var kls = msg.good ? 'right' : 'wrong';
            m.dom.cells.forEach(cell => {
                if (msg.idxs.indexOf(+cell.dataset.idx) !== -1) cell.classList.add(kls);
            });
            if (!msg.good) setTimeout(() => {
                m.dom.cells.forEach(cell => cell.classList.remove(kls));
            }, 200);

        }

    };

})();
