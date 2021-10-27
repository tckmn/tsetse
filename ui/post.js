window.addEventListener('load', () => {
    for (k in m) if (m[k]._onload) m[k]._onload();
});
