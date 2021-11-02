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

    var ex = document.getElementById('FO1Dex');
    card(ex, m.FO1D.render([[1, false], [8, true]]));
    card(ex, m.FO1D.render([[2, true], [9, false]]));
    card(ex, m.FO1D.render([[4, true], [7, true]]));

    var ex = document.getElementById('FOLDex');
    card(ex, m.FOLD.render([[2, true], [3, true]]));
    card(ex, m.FOLD.render([[4, false], [1, true]]));
    card(ex, m.FOLD.render([[5, true], [5, false]]));

    var ex = document.getElementById('OCTAex');
    card(ex, m.OCTA.render([[2,1,0,3], true]));
    card(ex, m.OCTA.render([[3,2,1,0], true]));
    card(ex, m.OCTA.render([[0,3,2,1], true]));

    var ex = document.getElementById('A5SETex1');
    card(ex, m.A5SET.render([3,1,4,0,2]));
    // card(ex, m.A5SET.render([2,3,0,1,4]));
    card(ex, m.A5SET.render([4,2,1,3,0]));
    // card(ex, m.A5SET.render([0,4,3,2,1]));
    card(ex, m.A5SET.render([1,0,2,4,3]));

    var ex = document.getElementById('A5SETex2');
    card(ex, m.A5SET.render([3,2,0,4,1]));
    card(ex, m.A5SET.render([3,1,0,2,4]));
    card(ex, m.A5SET.render([3,4,0,1,2]));

});
