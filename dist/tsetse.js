var $ = document.querySelector.bind(document),
    $$ = document.querySelectorAll.bind(document),
    ws;

window.addEventListener('load', () => {

    var cells = $$('div.cell'),
        cltlist = $('#cltlist'), strikelist = $$('.strike'),
        offsets = [], offset = 0,
        latencies = [], latency = 0,
        startTime = 0, duration = 0, timerIntr;

    ws = new WebSocket('ws://' + location.hostname + ':9255/');
    var send = (t, obj) => ws.send(JSON.stringify({...obj, t: t}));

    ws.onopen = () => {
        var userinfo = localStorage.getItem('userinfo');
        if (userinfo) send('Identify', JSON.parse(userinfo));
        // else send('Register', { uname: prompt('enter a username') });
        else send('Register', { uname: 'tckmn' });
    };

    ws.onmessage = e => {
        var msg = JSON.parse(e.data); handlers[msg.t](msg);
    };

    ws.onclose = () => { $('#discon').style.display = 'block'; };


    var svgel = (name, props) => {
        var el = document.createElementNS('http://www.w3.org/2000/svg', name);
        if (props) for (prop in props) {
            el.setAttribute(prop[0] == '_' ?
                prop.slice(1) :
                prop.replace(/[A-Z]/g, m => '-'+m.toLowerCase()),
                props[prop]);
        }
        return el;
    };

    var handlers = {

        Registered: msg => {
            localStorage.setItem('userinfo', JSON.stringify({
                cid: msg.cid,
                secret: msg.secret
            }));
        },

        Identified: msg => {
        },

        NotIdentified: msg => {
            localStorage.clear();
            location.reload();
        },

        Cards: msg => {

            var five = [0,1,2,3,4],
                colors = ['#f00', '#90f', '#00f', '#0d0', '#f09600'],
                sep = 2.5,
                pentW = 0.2, lineW = 0.2, outlineW = 0.1;

            msg.cards.forEach((card, idx) => {
                var svg = svgel('svg', {
                    _viewBox: `-1 -${sep/2} 2 ${sep*3}`
                });
                card.forEach((p, i) => {
                    var x = j => Math.sin(Math.PI*2/5*j),
                        y = j => -Math.cos(Math.PI*2/5*j)+sep*i,
                        stroke = ['#aaa','#666','#000'][i];

                    // the pentagon
                    svg.appendChild(svgel('path', {
                        d: five.map(j => `${j?'L':'M'} ${x(j)} ${y(j)}`).join(' ') + 'Z',
                        stroke: stroke, strokeWidth: pentW, fill: 'transparent'
                    }));

                    // outline of line from center to point
                    svg.appendChild(svgel('path', {
                        d: `M 0 ${sep*i} L ${x(p)} ${y(p)}`,
                        stroke: stroke, strokeWidth: lineW + 2*outlineW
                    }));

                    // circles at points
                    five.forEach(j => {
                        svg.appendChild(svgel('circle', {
                            cx: x(j), cy: y(j), r: 0.2,
                            stroke: stroke, strokeWidth: outlineW, fill: colors[j]
                        }));
                    });

                    // circle at center
                    svg.appendChild(svgel('circle', {
                        cx: 0, cy: sep*i, r: 0.2,
                        stroke: stroke, strokeWidth: outlineW, fill: colors[p]
                    }));

                    // line from center to point
                    svg.appendChild(svgel('path', {
                        d: `M 0 ${sep*i} L ${x(p)} ${y(p)}`,
                        stroke: colors[p], strokeWidth: lineW
                    }));
                });
                cells[idx].appendChild(svg);
                // cells[idx].textContent = card.join(',');
            });

        },

    };

                    // case 'a':
                    //     if (data === '1') {
                    //         console.log('you are an admin');
                    //         $('#admin').style.display = 'block';
                    //     } else {
                    //         console.log('you are not an admin');
                    //         $('#admin').style.display = 'none';
                    //     }
                    //     break;
                    // case 'p':
                    //     $('#key').classList.toggle('player', isPlayer = data === '1');
                    //     break;
                    // case 'c':
                    //     while (cltlist.lastChild) cltlist.removeChild(cltlist.lastChild);
                    //     data.split('/').forEach(clt => {
                    //         var div = document.createElement('div');
                    //         div.appendChild(document.createTextNode(clt + ' '));
                    //         var btn1 = document.createElement('button'),
                    //             btn2 = document.createElement('button');
                    //         btn1.textContent = 'p';
                    //         btn2.textContent = 'a';
                    //         [btn1, btn2].forEach(btn => {
                    //             btn.addEventListener('click', () => {
                    //                 ws.send(toggleFlag(btn.textContent, clt));
                    //             });
                    //             div.appendChild(btn);
                    //         });
                    //         cltlist.appendChild(div);
                    //     });
                    //     break;
                    // case 'w':
                    //     // make sure to keep this in sync with ABC in netwall.hs
                    //     wall = data.split('\n');
                    //     groups = [];
                    //     strikes = 3;
                    //     startTime = +wall.shift();
                    //     duration = +wall.shift();
                    //     render();
                    //     renderStrikes();
                    //     resetTimer();
                    //     break;
                    // case 'W':
                    //     // make sure to keep this in sync with DEF in netwall.hs
                    //     wall = [];
                    //     groups = [];
                    //     strikes = 3;
                    //     startTime = 0;
                    //     render();
                    //     renderStrikes();
                    //     resetTimer();
                    //     break;
                    // case 'g':
                    //     groups = data.split('/').map(x => +x);
                    //     render();
                    //     break;
                    // case 'G':
                    //     flash(data.split('/').map(x => +x));
                    //     break;
                    // case 's':
                    //     strikes = +data;
                    //     renderStrikes();
                    //     break;
                    // case 't':
                    //     timeSync(data);
                    //     break;

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
        clr: () => { ws.send('W'); }
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
            if (true) {
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
