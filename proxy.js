function FindProxyForURL (url, host) {
    // Mines URLS need the proxy
    if (shExpMatch(host, '*.mines.edu')
        || shExpMatch(host, '*.mines.edu:*') ) {
        return 'SOCKS localhost:1080';
    }

    // Don't proxy the whole intarweb
    return 'DIRECT';
}
