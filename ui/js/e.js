m.e = (function() {

    return {

        _onload: function() {
            'wrap wallwrap wall gamelist name discon showadmin sbmain sbconf sbtoggle lobby helplink claim pluscard'.split(' ').forEach(id => this[id] = document.getElementById(id));

            if (localStorage.getItem('password')) this.wrap.classList.add('hasadmin');

            this.name.addEventListener('click', () => m.modal.show('namechange'));
            this.showadmin.addEventListener('click', () => m.modal.show('admin'));

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
