var gallery = [
    {
        img: 'cset1.jpg',
        caption: 'The very first printed deck. This was an early prototype of C53T with different card styles; we also originally played in "easy mode" with the cards sorted by one of the pentagons.',
        date: '2021-09-18'
    },
    {
        img: 'firstcset.jpg',
        caption: 'One of the first C53Ts ever found.',
        date: '2021-09-18'
    },
    {
        img: 'voting1.jpg',
        caption: 'An early snapshot of the table of proposals for set variants, which has since gone through many additions and changes.',
        date: '2021-09-22'
    },
    {
        img: 'houseofcards.jpg',
        caption: 'A house of cards. (Can you find a set in each "room"?)',
        date: '2021-09-30'
    },
    {
        img: 'zendo.jpg',
        caption: 'I wrote these examples on the whiteboard with no other explanation, and posed it as a puzzle for everyone else to figure out.',
        date: '2021-10-01'
    },
    {
        img: 'speedrun.jpg',
        caption: 'The result of my speedrun through a full C53T deck, clocking in at 51:28.90. The fastest single set was 21.36, including grabbing the set and redealing.',
        date: '2021-10-05'
    },
    {
        img: 'behindthescenes.jpg',
        caption: 'Behind the scenes of the process of making tsetse, checking to make sure the cards match the real-life ones.',
        date: '2021-11-02'
    }
];

function card(ex, x) {
    var c = m.dom.el('div', { class: 'helpcard' });
    c.appendChild(x);
    ex.appendChild(c);
}

window.addEventListener('load', () => {

    var ex = document.getElementById('SETex');
    card(ex, m.SET.render([0,1,0,1]));
    card(ex, m.SET.render([1,1,1,1]));
    card(ex, m.SET.render([2,1,2,1]));

    var ex = document.getElementById('PROex');
    card(ex, m.PRO.render([true,true,false,false,true,true]));
    card(ex, m.PRO.render([true,false,false,false,false,false]));
    card(ex, m.PRO.render([false,true,true,true,false,true]));
    card(ex, m.PRO.render([false,false,true,true,true,false]));

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
    card(ex, m.A5SET.render([4,2,1,3,0]));
    card(ex, m.A5SET.render([1,0,2,4,3]));

    var ex = document.getElementById('A5SETex2');
    card(ex, m.A5SET.render([3,2,0,4,1]));
    card(ex, m.A5SET.render([3,1,0,2,4]));
    card(ex, m.A5SET.render([3,4,0,1,2]));

    var ex = document.getElementById('S3CTex');
    card(ex, m.S3CT.render([3,4,5,0,1,2]));
    card(ex, m.S3CT.render([1,0,2,4,5,3]));
    card(ex, m.S3CT.render([5,4,3,0,2,1]));

    var ex = document.getElementById('C3C3ex');
    card(ex, m.C3C3.render([0,[0,0,0]]));
    card(ex, m.C3C3.render([1,[2,0,1]]));
    card(ex, m.C3C3.render([2,[2,1,0]]));

    var gprev = document.getElementById('gprev'),
        gnext = document.getElementById('gnext'),
        gdate = document.getElementById('gdate'),
        gcap = document.getElementById('gcap'),
        gimg = document.getElementById('gimg'),
        gcur = 0,
        ggo = diff => {
            gcur = (gcur + diff + gallery.length) % gallery.length;
            var url = '/img/gallery/'+gallery[gcur].img,
                a = m.dom.el('a', { href: url, target: '_blank' });
            gcap.innerText = gallery[gcur].caption;
            gdate.innerText = gallery[gcur].date;
            m.dom.clr(gimg);
            a.appendChild(m.dom.el('img', {
                src: url, style: 'max-height: 400px; max-width: 100%'
            }));
            gimg.appendChild(a);
        };
    gprev.addEventListener('click', () => ggo(-1));
    gnext.addEventListener('click', () => ggo(1));
    ggo(0);

});
