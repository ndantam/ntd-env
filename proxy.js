function FindProxyForURL (url, host) {
    var proxy='SOCKS localhost:1080';
    var domains = [
        // Mines URLS get proxied
        '.mines.edu',
        // Proxy publication sites to get Mines access
        'scholar.google.com',
        'ieeexplore.ieee.org'
    ];

    if (domains.find( domain => dnsDomainIs(host, domain))) {
        return proxy;
    }

    // Don't proxy the whole intarwebs
    return 'DIRECT';
}
