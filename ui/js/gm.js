m.gm = (function() {

    return {

        Registered: function(msg) {
            localStorage.setItem('userinfo', JSON.stringify({
                cid: msg.cid,
                secret: msg.secret
            }));
            m.cid = msg.cid;
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
                    var tr = m.dom.el('tr', { id: 'userlist-' + u.uid });
                    if (u.conn) tr.classList.add('conn');
                    if (u.play) tr.classList.add('play');
                    tr.appendChild(m.dom.el('td', { class: 'score', text: u.score }));
                    tr.appendChild(m.dom.el('td', { class: 'name', text: u.name }));
                    tbl.appendChild(tr);
                });

            m.e.sbmain.appendChild(tbl);

        },

        GameList: function(msg) {

            m.dom.clr(m.e.gamelist);

            var newsec = m.dom.el('div');
            newsec.appendChild(m.dom.el('button', {
                text: 'create game',
                onclick: () => m.modal.show('newgame')
            }));
            m.e.gamelist.appendChild(newsec);

            msg.list.forEach(g => {
                var gamerect = m.dom.el('div', {
                    class: 'gamerect' + (g.dead ? ' dead' : ''),
                    onclick: () => {
                        m.net.send('JoinGame', { gid: g.gid });
                    }
                });
                var gameimg = m.dom.el('div', { class: 'gameimg' });
                gameimg.appendChild(m[g.gtype].img(m.util.srand(g.creation), g.conf));
                gamerect.appendChild(gameimg);
                gamerect.appendChild(m.dom.el('span', { text: g.gtype, class: 'gamename' }));
                gamerect.appendChild(m.dom.el('span', { text: g.gdesc, class: 'gamedesc' }));
                gamerect.appendChild(m.dom.el('span', { text: `by ${g.creator} at ${new Date(g.creation).toLocaleString()}`, class: 'gamesrc' }));
                m.e.gamelist.appendChild(gamerect);
            });

        },

        GameType: function(msg) {
            m.dom.clr(m.e.wall);
            m.dom.clr(m.e.sbmain);
            m.dom.cells = [];

            m.conf.deinit();
            m.game = msg.gtype;
            m.gconf = msg.conf;
            m.conf.init();

            // bit of an ugly hack to put this here
            m.e.wrap.classList.toggle('hasgame', !!m.game);
            m.e.helplink.setAttribute('target', m.game ? '_blank' : '_self');
            m.e.helplink.setAttribute('href', '/help.html' + (m.game ? '#'+m.game : ''));
            m.e.helplink.textContent = m.game ? 'rules' : 'help/about';
        },

        Scores: function(msg) {
            var table = m.dom.el('table'),
                games = Object.keys(msg.scores);

            table.appendChild(m.dom.el('tr', {
                children: [m.dom.el('td')].concat(games.map(g => m.dom.el('th', { text: g })))
            }));

            new Set([].concat.apply([], Object.values(msg.scores).map(Object.keys))).forEach(u => {
                table.appendChild(m.dom.el('tr', {
                    children: [m.dom.el('th', { text: u, class: 'name' })].concat(games.map(g => m.dom.el('td', { text: msg.scores[g][u] || '' })))
                }));
            });

            m.dom.clr(m.e.scoresbody);
            m.e.scoresbody.appendChild(table);
            m.modal.show('scores');
        },

        Toast: function(msg) {
            m.dom.toast(msg.msg);
        }

    };

})();
