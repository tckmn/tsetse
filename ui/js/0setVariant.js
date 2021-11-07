m.setVariant = (function() {

    return {

        props: function(args) {
            return {

                Cards: function(msg) {
                    m.dom.clearCells();
                    msg.cards.forEach((card, idx) => {
                        m.dom.addCell(this.render(card), idx, args.autosubmit || Infinity);
                    });
                },

                Highlight: function(msg) {
                    var kls = msg.good ? 'right' : 'wrong';
                    m.dom.cells.forEach(cell => {
                        if (msg.idxs.indexOf(+cell.dataset.idx) !== -1) cell.classList.add(kls);
                    });
                    if (!msg.good) setTimeout(() => {
                        m.dom.cells.forEach(cell => cell.classList.remove(kls));
                    }, 200);
                },

                History: function(msg) {
                    m.dom.clr(m.e.histbody);
                    msg.history.forEach(([uid, cards, time]) => {
                        m.e.histbody.appendChild(m.dom.el('p', {
                            text: `${uid} at ${new Date(time).toLocaleString()}`
                        }));
                        var cdiv = m.dom.el('div');
                        cards.forEach(card => {
                            var c = m.dom.el('div', { class: 'helpcard' });
                            c.appendChild(m[m.game].render(card));
                            cdiv.appendChild(c);
                        });
                        m.e.histbody.appendChild(cdiv);
                    });
                    m.modal.show('hist');
                },

                conf: {
                    rownum: args.perrow ? { default: args.perrow } : {},
                    square: {},
                    richter: {},
                    ...args.conf
                }

            };
        }

    };

})();
