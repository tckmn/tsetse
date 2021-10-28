m.e = (function() {

    return {

        _onload: function() {
            'wrap wallwrap wall gamelist name discon sbmain sbconf lobby helplink'.split(' ').forEach(id => this[id] = document.getElementById(id));

            this.lobby.addEventListener('click', e => {
                e.preventDefault();
                m.net.send('JoinGame', { gid: -1 });
            });
        }

    };

})();
