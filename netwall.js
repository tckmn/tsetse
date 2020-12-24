var $ = document.querySelector.bind(document),
    $$ = document.querySelectorAll.bind(document);

var NEGOTIATE = 0, PLAY = 1, ERROR = -1;

var ws;

window.addEventListener('load', () => {

    var stage = NEGOTIATE, retrycount = 0,
        cells = $$('div.cell'),
        cltlist = $('#cltlist'), strikelist = $$('.strike'),
        wall = [], groups = [], strikes = 3,
        isPlayer = false,
        offsets = [], offset = 0,
        latencies = [], latency = 0,
        startTime = 0, duration = 0, timerIntr;

    ws = new WebSocket('ws://a.tck.mn:9255/');

    var negotiate = regen => {
        var key = (regen !== true && localStorage.getItem('key')) || genkey();
        ws.send(key + Math.round(new Date()));
    };
    ws.onopen = negotiate;

    ws.onmessage = e => {
        var data = e.data.slice(1);
        switch (stage) {

            case NEGOTIATE:
                switch (e.data[0]) {
                    case 'g':
                        $('#key').textContent = localStorage.getItem('key').slice(0,5);
                        timeSync(data);
                        stage = PLAY;
                        break;
                    case 'r':
                        if (retrycount++ < 10) {
                            negotiate(true);
                            break;
                        }
                        //fallthrough
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
                        // make sure to keep this in sync with ABC in netwall.hs
                        wall = data.split('\n');
                        groups = [];
                        strikes = 3;
                        startTime = +wall.shift();
                        duration = +wall.shift();
                        render();
                        renderStrikes();
                        resetTimer();
                        break;
                    case 'g':
                        groups = data.split('/').map(x => +x);
                        render();
                        break;
                    case 'G':
                        flash(data.split('/').map(x => +x));
                        break;
                    case 's':
                        strikes = +data;
                        renderStrikes();
                        break;
                    case 't':
                        timeSync(data);
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
        cells.forEach((cell, idx) => {
            var realidx = groups[idx] !== undefined ? groups[idx] : rest[idx-groups.length];
            cell.textContent = wall[realidx];
            cell.dataset.idx = realidx;
            cell.classList.toggle('solved', idx < groups.length);
            cell.classList.remove('selected');
        });
    };

    var renderStrikes = () => {
        for (var i = 0; i < 3; ++i) {
            strikelist[i].classList.toggle('used', strikes < 3-i);
        }
    };

    var resetTimer = () => {
        if (timerIntr !== undefined) clearInterval(timerIntr);

        var adjusted = startTime + offset,
            now = Math.round(new Date()),
            elapsed = now - adjusted,
            secsElapsed = Math.floor(elapsed/1000),
            millisElapsed = elapsed - secsElapsed*1000;

        setTimer(duration - secsElapsed);
        setTimeout(() => {
            if (timerIntr !== undefined) clearInterval(timerIntr);
            var expected = duration - secsElapsed - 1;
            setTimer(expected);
            timerIntr = setInterval(() => {
                setTimer(--expected);
                if (expected <= 0 && timerIntr !== undefined) clearInterval(timerIntr);
            }, 1000);
        }, 1000 - millisElapsed);
    };

    var setTimer = secs => {
        if (secs < 0) secs = 0;
        pctLeft = 100*secs/duration;
        timer.innerText = Math.floor(secs/60) + ':' + padsec(secs%60);
        timer.style.background = 'linear-gradient(to right, #43615d 0 '+pctLeft+'%, #000 '+pctLeft+'% 100%)';
    };

    var flash = idxs => {
        cells.forEach(cell => {
            if (idxs.indexOf(+cell.dataset.idx) !== -1) cell.classList.add('wrong');
        });
        setTimeout(() => {
            cells.forEach(cell => cell.classList.remove('wrong'));
        }, 200);
    };

    var timeSync = data => {
        // currently we don't use latency for anything,
        // but maybe we should account for it in the timer

        var parts = data.split('/').map(x => +x),
            now = Math.round(new Date());

        insertSorted(offsets, (now+parts[0])/2 - parts[1]);
        insertSorted(latencies, now - parts[0]);
        offset = offsets[offsets.length/2 | 0];
        latency = latencies[latencies.length/2 | 0];

        console.log('offsets', offsets, offset, 'latencies', latencies, latency);

        if (offsets.length < 10) setTimeout(() => {
            ws.send('t' + Math.round(new Date()));
        }, 3000);
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

function insertSorted(arr, val) {
    for (var i = 0; i < arr.length; ++i) {
        if (val < arr[i]) {
            arr.splice(i, 0, val);
            return;
        }
    }
    arr.push(val);
}

function padsec(sec) {
    return sec < 10 ? '0'+sec : sec;
}
