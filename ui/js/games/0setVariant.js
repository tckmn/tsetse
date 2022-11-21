m.setVariant = (function() {

    return {

        props: function(args) {
            args = args || {};
            return {

                Cards: function(msg) {
                    m.dom.clearCells();
                    msg.cards.forEach((card, idx) => {
                        m.dom.addCell(this.render(card, m.gconf), idx, args.autosubmit || Infinity);
                    });
                    // TODO what a hack
                    document.querySelectorAll('div#sbmain table.userlist tr').forEach(x => x.classList.remove('right'));
                },

                Highlight: function(msg) {
                    if (!msg.good && msg.who !== m.cid) return;
                    var kls = msg.good ? 'right' : 'wrong',
                        uel = document.getElementById('userlist-' + msg.who);

                    m.dom.eachCell(cell => {
                        if (msg.idxs.indexOf(+cell.dataset.idx) !== -1) cell.classList.add(kls);
                    });
                    if (uel) uel.classList.add(kls);

                    if (!msg.good) setTimeout(() => {
                        m.dom.eachCell(cell => cell.classList.remove(kls));
                        if (uel) uel.classList.remove(kls);
                    }, 200);
                },

                History: function(msg) {
                    m.dom.clr(m.e.histbody);
                    var prevtime;
                    msg.history.forEach(([uid, cards, time]) => {
                        var cdiv = m.dom.el('div');
                        cards.forEach(card => {
                            var c = m.dom.el('div', { class: 'helpcard' });
                            c.appendChild(this.render(card, m.gconf));
                            cdiv.appendChild(c);
                        });
                        m.e.histbody.insertBefore(cdiv, m.e.histbody.firstChild);

                        time = new Date(time);
                        m.e.histbody.insertBefore(m.dom.el('p', {
                            text: `${uid} at ${time.toLocaleString()}` +
                                (prevtime ? ` in ${m.util.stt((time - prevtime) / 1000)}` : '')
                        }), m.e.histbody.firstChild);
                        prevtime = time;
                    });
                    m.modal.show('hist');
                },

                defaultConfig: {
                    boardSize: args.boardSize || 12,
                    dealDelay: args.dealDelay || 5000,
                    maxRedeals: args.maxRedeals || 50,
                    subconf: args.defaultConfig || []
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
