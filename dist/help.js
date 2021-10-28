function card(ex, x) {
    var c = m.dom.el('div', { class: 'helpcard' });
    c.appendChild(x);
    ex.appendChild(c);
}

window.addEventListener('load', () => {

    var ex = document.getElementById('C53Tex');
    card(ex, m.C53T.render([0,2,0]));
    card(ex, m.C53T.render([1,4,2]));
    card(ex, m.C53T.render([0,1,3]));
    card(ex, m.C53T.render([0,2,1]));
    card(ex, m.C53T.render([4,1,4]));

});
