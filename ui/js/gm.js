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
            'C53T FOLD FO1D OCTA A5SET S3CT C3C3'.split(' ').forEach(g => {
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
                var gameimg = m.dom.el('div', { class: 'gameimg' });
                gameimg.appendChild(m[g[1][0]].img(m.util.srand(JSON.stringify(g))));
                gamerect.appendChild(gameimg);
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

            m.conf.deinit();
            m.game = msg.gtype;
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
            // can't use onclick here because they're mutually recursive, lol
            var toast = m.dom.el('div', {
                class: 'toast',
                text: msg.msg
            }), close = () => {
                if (toast) document.body.removeChild(toast);
                toast = undefined;
            };
            toast.addEventListener('click', close);
            document.body.appendChild(toast);
            setTimeout(close, Math.max(5000, msg.msg.length*200));
        }

    };

})();
