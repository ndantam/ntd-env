function FindProxyForURL (url, host) {
    var proxy='SOCKS localhost:1080';
    var domains = [
        // Mines URLS get proxied
        '.mines.edu',
        // Proxy publication sites to get Mines access
        'scholar.google.com',
        'ieeexplore.ieee.org',
        'sciencedirect.com',
        'onlinelibrary.wiley.com',
        'dl.acm.org',
        'journals.sagepub.com',
        'link.springer.com',
        'science.org',
        'nature.com',
        'cambridge.org'
    ];

    // No proxy on Mines IP addresses
    // Mines subnets from AS36704 (https://bgpview.io/asn/36704#prefixes-v4)
    if (isInNet(myIpAddress(), "138.67.0.0", "255.255.0.0")) {
        return 'DIRECT';
    }

    if (domains.find( domain => dnsDomainIs(host, domain))) {
        return proxy;
    }

    // Don't proxy the whole intarwebs
    return 'DIRECT';
}
