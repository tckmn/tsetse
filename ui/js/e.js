m.e = (function() {

    return {

        _onload: function() {

            'wrap wallwrap wall showdead gamelist name discon showadmin showhist showscores histbody scoresbody sbmain sbconf sbtoggle lobby helplink claim pluscard gametype gameconfig confreset'.split(' ').forEach(id => this[id] = document.getElementById(id));

            if (localStorage.getItem('password')) this.wrap.classList.add('hasadmin');

            // TODO all the showdead nonsense should be abstracted into something similar to m.conf
            this.showdead.addEventListener('change', () => m.net.rerun('GameList'));

            this.name.addEventListener('click', () => m.modal.show('namechange'));
            this.showadmin.addEventListener('click', () => m.modal.show('admin'));

            this.showhist.addEventListener('click', () => m.net.send('GetHistory', {}));
            this.showscores.addEventListener('click', e => { e.preventDefault(); m.net.send('GetScores'); });

            this.lobby.addEventListener('click', e => {
                e.preventDefault();
                m.net.send('JoinGame', { gid: -1 });
            });

            this.sbtoggle.addEventListener('click', () => {
                this.wrap.classList.toggle('hidebar');
            });

            this.claim.addEventListener('click', () => {
                m.dom.submitCells();
            });

            this.pluscard.addEventListener('click', () => {
                m.net.send('PlusCard', {});
            });

            // TODO maybe this belongs elsewhere
            'SET C53T FOLD FO1D OCTA A5SET S3CT C3C3 SAT OC'.split(' ').forEach(g => {
                var el = m.dom.el('div', {
                    children: [
                        m.dom.el('div', {
                            class: 'gameimg',
                            children: [m[g].img(m.util.srand(''), m[g].defaultConfig)]
                        }),
                        g
                    ],
                    onclick: e => {
                        var prev = document.getElementById('gtchoice');
                        if (prev) prev.removeAttribute('id');
                        el.setAttribute('id', 'gtchoice');
                        this.gameconfig.value = JSON.stringify(m[g].defaultConfig);
                    }
                });
                el.dataset.g = g;
                this.gametype.appendChild(el);
            });

            this.confreset.addEventListener('click', e => {
                e.preventDefault();
                var g = document.getElementById('gtchoice');
                if (g) this.gameconfig.value = JSON.stringify(m[g.dataset.g].defaultConfig);
            });

        }

    };

})();
