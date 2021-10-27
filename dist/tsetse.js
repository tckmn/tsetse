var $ = document.querySelector.bind(document),
    $$ = document.querySelectorAll.bind(document),
    ws;

window.addEventListener('load', () => {

    var e = {},
        cells = [],
        offsets = [], offset = 0,
        latencies = [], latency = 0,
        startTime = 0, duration = 0, timerIntr;

    'wall wallwrap name discon sbmain rownum rowsub rowadd square lobby'.split(' ').forEach(id => e[id] = document.getElementById(id));

    ws = new WebSocket('ws://' + location.hostname + ':9255/');
    var send = (t, obj) => ws.send(JSON.stringify({...obj, t: t}));

    ws.onopen = () => {
        var userinfo = localStorage.getItem('userinfo');
        if (userinfo) send('Identify', JSON.parse(userinfo));
        else send('Register', { uname: prompt('enter a username') });
    };

    ws.onmessage = ev => {
        var msg = JSON.parse(ev.data); handlers[msg.t](msg);
    };

    ws.onclose = () => {
        e.name.style.display = 'none';
        e.discon.style.display = 'block';
    };

    var resize = () => {
        var rect = e.wallwrap.getBoundingClientRect();
        e.wall.style.maxWidth = settings.square.value ? rect.height + 'px' : '';
        e.wall.style.maxHeight = settings.square.value ? rect.width + 'px' : '';
    };
    window.addEventListener('resize', resize);

    var settings = {

        offset: {
        },

        filled: {
        },

        rownum: {
            default: 4, min: 1, max: 12,
            dec: e.rowsub, inc: e.rowadd,
            update: n => {
                e.rownum.textContent = n;
                e.wall.style.gridTemplateColumns = `repeat(${n},1fr)`;
            }
        },

        square: {
            default: true,
            box: e.square,
            update: b => {
                e.square.checked = b;
                resize();
            }
        }

    };

    var saveSettings = () => {
        var obj = {};
        for (key in settings) obj[key] = settings[key].value;
        localStorage.setItem('settings', JSON.stringify(obj));
    };

    var ls = localStorage.getItem('settings');
    ls = ls ? JSON.parse(ls) : {};
    for (key in settings) { (function(key) {
        var obj = settings[key];
        obj.update(obj.value = ls[key] === undefined ? obj.default : ls[key]);
        if (obj.inc) obj.inc.addEventListener('click', () => {
            obj.update(obj.value = Math.min(obj.max, obj.value+1));
            saveSettings();
        });
        if (obj.dec) obj.dec.addEventListener('click', () => {
            obj.update(obj.value = Math.max(obj.min, obj.value-1));
            saveSettings();
        });
        if (obj.box) obj.box.addEventListener('change', () => {
            obj.update(obj.value = obj.box.checked);
            saveSettings();
        });
    })(key); }

    e.lobby.addEventListener('click', ev => {
        ev.preventDefault();
        send('JoinGame', { gid: -1 });
    });

    var el = (name, props) => {
        var el = document.createElement(name);
        if (props) for (prop in props) {
            if (prop === 'text') el.appendChild(document.createTextNode(props.text));
            else if (prop.slice(0,2) === 'on') el.addEventListener(prop.slice(2), props[prop]);
            else el.setAttribute(prop, props[prop]);
        }
        return el;
    };

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

    var clr = el => {
        while (el.firstChild) el.removeChild(el.firstChild);
    };

    var addCell = (content, idx) => {
        var cell = document.createElement('div');
        cell.dataset.idx = idx;
        cell.classList.add('cell');
        cell.appendChild(content);
        e.wall.appendChild(cell);
        cells.push(cell);

        fn = ev => {
            ev.preventDefault();
            if (true) {
                cell.classList.toggle('selected');
                var guesses = Array.from($$('.selected'));
                if (guesses.length === 5) {
                    guesses.forEach(g => g.classList.remove('selected'));
                    send('Claim', {
                        idxs: guesses.map(g => +g.dataset.idx)
                    });
                }
            }
        };
        cell.addEventListener('mousedown', fn);
        cell.addEventListener('touchstart', fn);
    };

    var handlers = {

        Registered: msg => {
            localStorage.setItem('userinfo', JSON.stringify({
                cid: msg.cid,
                secret: msg.secret
            }));
            e.name.textContent = msg.name;
        },

        Identified: msg => {
            e.name.textContent = msg.name;
        },

        NotIdentified: msg => {
            localStorage.clear();
            location.reload();
        },

        UserList: msg => {

            clr(e.sbmain);

            var tbl = el('table', { 'class': 'userlist' });

            msg.list
                .sort((a,b) => (b.score - (b.conn?0:999)) - (a.score - (a.conn?0:999)))
                .forEach(u => {
                    var tr = el('tr');
                    if (u.conn) tr.classList.add('conn');
                    if (u.play) tr.classList.add('play');
                    tr.appendChild(el('td', { text: u.score }));
                    tr.appendChild(el('td', { text: u.name }));
                    tbl.appendChild(tr);
                });

            e.sbmain.appendChild(tbl);

        },

        GameList: msg => {

            clr(e.wall);
            cells = [];

            msg.list.forEach(g => {
                e.wall.appendChild(el('a', {
                    text: g[1], href: '#',
                    onclick: ev => {
                        ev.preventDefault();
                        send('JoinGame', { gid: g[0] });
                    }
                }));
            });

            var sel = el('select');
            'c53t'.split(' ').forEach(g => {
                sel.appendChild(el('option', {
                    text: g, value: g
                }));
            });

            var newsec = el('div');
            newsec.appendChild(sel);
            newsec.appendChild(el('button', {
                text: 'create game',
                onclick: ev => {
                    send('CreateGame', { gtype: sel.value });
                }
            }));

            e.wall.appendChild(newsec);

        },

        Cards: msg => {

            clr(e.wall);
            cells = [];

            var five = [0,1,2,3,4],
                colors = ['#f00', '#90f', '#00f', '#0d0', '#f09600'],
                xsep = 1, ysep = 2.5,
                pentW = 0.2, lineW = 0.2, outlineW = 0.06;

            msg.cards.forEach((card, idx) => {

                var svg = svgel('svg', {
                    _viewBox: `-${xsep+1.5} -${ysep/2+0.2} ${xsep*2+3} ${ysep*3+0.2}`
                });

                // svg.appendChild(svgel('rect', {x:-1.5,y:-sep/2,width:3,height:sep*3,fill:'red'}));

                card.forEach((p, i) => {
                    var cx = i*xsep-xsep,
                        cy = i*ysep,
                        x = j => cx + Math.sin(Math.PI*2/5*j),
                        y = j => cy - Math.cos(Math.PI*2/5*j),
                        stroke = ['#aaa','#666','#000'][i];

                    // the pentagon
                    svg.appendChild(svgel('path', {
                        d: five.map(j => `${j?'L':'M'} ${x(j)} ${y(j)}`).join(' ') + 'Z',
                        stroke: stroke, strokeWidth: pentW, fill: ['#ddd', '#999', '#333'][i]
                    }));

                    // outline of line from center to point
                    svg.appendChild(svgel('path', {
                        d: `M ${cx} ${cy} L ${x(p)} ${y(p)}`,
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
                        cx: cx, cy: cy, r: 0.2,
                        stroke: stroke, strokeWidth: outlineW, fill: colors[p]
                    }));

                    // line from center to point
                    svg.appendChild(svgel('path', {
                        d: `M ${cx} ${cy} L ${x(p)} ${y(p)}`,
                        stroke: colors[p], strokeWidth: lineW
                    }));
                });

                addCell(svg, idx);

            });

        },

        Highlight: msg => {

            var kls = msg.good ? 'right' : 'wrong';
            cells.forEach(cell => {
                if (msg.idxs.indexOf(+cell.dataset.idx) !== -1) cell.classList.add(kls);
            });
            if (!msg.good) setTimeout(() => {
                cells.forEach(cell => cell.classList.remove(kls));
            }, 200);

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
