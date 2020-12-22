var $ = document.querySelector.bind(document),
    $$ = document.querySelectorAll.bind(document);

var NEGOTIATE = 0, PLAY = 1, ERROR = -1;

var ws;

window.addEventListener('load', () => {

    var stage = NEGOTIATE, retrycount = 0,
        cells = $$('div.cell'),
        cltlist = $('#cltlist'),
        wall = [], groups = [],
        isPlayer = false;

    ws = new WebSocket('ws://a.tck.mn:9255/');

    var negotiate = regen => {
        var key = (regen !== true && localStorage.getItem('key')) || genkey();
        ws.send(key);
    };
    ws.onopen = negotiate;

    ws.onmessage = e => {
        var data = e.data.slice(1);
        switch (stage) {

            case NEGOTIATE:
                switch (e.data) {
                    case 'g':
                        $('#key').textContent = localStorage.getItem('key').slice(0,5);
                        stage = PLAY;
                        break;
                    case 'r':
                        if (retrycount++ < 10) {
                            negotiate(true);
                            break;
                        }
                    default:
                        stage = ERROR;
                        alert(e.data === 'e' ? 'sorry, something weird happened' :
                              e.data === 'r' ? 'sorry, something very weird happened' :
                                               'sorry, something extremely weird happened');
                }
                break;

            case PLAY:
                switch (e.data[0]) {
                    case 'a':
                        if (data === '1') {
                            console.log('you are an admin');
                            $('#admin').style.display = 'block';
                        } else {
                            console.log('you are not an admin');
                            $('#admin').style.display = 'none';
                        }
                        break;
                    case 'p':
                        $('#key').classList.toggle('player', isPlayer = data === '1');
                        break;
                    case 'c':
                        while (cltlist.lastChild) cltlist.removeChild(cltlist.lastChild);
                        data.split('/').forEach(clt => {
                            var div = document.createElement('div');
                            div.appendChild(document.createTextNode(clt + ' '));
                            var btn1 = document.createElement('button'),
                                btn2 = document.createElement('button');
                            btn1.textContent = 'p';
                            btn2.textContent = 'a';
                            [btn1, btn2].forEach(btn => {
                                btn.addEventListener('click', () => {
                                    ws.send(toggleFlag(btn.textContent, clt));
                                });
                                div.appendChild(btn);
                            });
                            cltlist.appendChild(div);
                        });
                        break;
                    case 'w':
                        wall = data.split('\n');
                        groups = [];
                        render();
                        break;
                    case 'g':
                        groups = data.split('/').map(x => +x);
                        render();
                        break;
                    case 'G':
                        flash(data.split('/').map(x => +x));
                        break;
                }
                break;

        }
    };

    ws.onclose = () => {
        $('#discon').style.display = 'block';
    };

    var render = () => {
        var rest = Array.from(Array(16).keys()).filter(x => groups.indexOf(x) === -1);
        console.log(rest);
        cells.forEach((cell, idx) => {
            var realidx = groups[idx] !== undefined ? groups[idx] : rest[idx-groups.length];
            cell.textContent = wall[realidx];
            cell.dataset.idx = realidx;
            cell.classList.toggle('solved', idx < groups.length);
        });
    };

    var flash = idxs => {
        cells.forEach(cell => {
            if (idxs.indexOf(+cell.dataset.idx) !== -1) cell.classList.add('wrong');
        });
        setTimeout(() => {
            cells.forEach(cell => cell.classList.remove('wrong'));
        }, 200);
    };

    var actions = {
        set: () => { ws.send('w' + $('#settxt').value); },
        clr: () => { ws.send('w'); }
    };

    $$('#admin > span').forEach(link => {
        var modal = $('#' + link.id + 'm'), tmp;
        link.addEventListener('click', () => {
            modal.style.display = 'flex';
        });
        (tmp = modal.querySelector('.confirm')) && tmp.addEventListener('click', () => {
            actions[link.id]();
            modal.style.display = 'none';
        });
        modal.querySelector('.dismiss').addEventListener('click', () => {
            modal.style.display = 'none';
        });
    });

    cells.forEach((cell, idx) => {
        fn = e => {
            e.preventDefault();
            if (isPlayer && wall.length && idx >= groups.length) {
                cell.classList.toggle('selected');
                var guesses = Array.from($$('.selected'));
                if (guesses.length === 4) {
                    guesses.forEach(g => g.classList.remove('selected'));
                    ws.send('g' + guesses.map(g => g.dataset.idx).join('/'));
                }
            }
        };
        cell.addEventListener('mousedown', fn);
        cell.addEventListener('touchstart', fn);
    });

});

var alphabet = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
function genkey() {
    var key = Array(15).fill().map(() => alphabet[Math.random() * alphabet.length | 0]).join('');
    localStorage.setItem('key', key);
    return key;
}

function admin(pwd) {
    ws.send('a' + pwd);
}

function toggleFlag(flag, clt) {
    return flag.toUpperCase() + (clt.indexOf(flag) === -1 ? '1' : '0') + clt.substr(4,5);
}
