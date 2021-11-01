m.e = (function() {

    return {

        _onload: function() {
            'wrap wallwrap wall gamelist name discon sbmain sbconf sbtoggle lobby helplink claim pluscard'.split(' ').forEach(id => this[id] = document.getElementById(id));

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
        }

    };

})();
