window.addEventListener('load', () => {
    if (location.pathname === '/') {
        for (k in m) if (m[k]._onload) m[k]._onload();
    }
});
