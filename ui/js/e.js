m.e = (function() {

    return {

        _onload: function() {
            'wall wallwrap name discon sbmain sbconf lobby'.split(' ').forEach(id => this[id] = document.getElementById(id));

            this.lobby.addEventListener('click', e => {
                e.preventDefault();
                m.net.send('JoinGame', { gid: -1 });
            });
        }

    };

})();
