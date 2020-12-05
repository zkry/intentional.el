function currentSiteAllowed(sites) {
    let atSite = window.location.href;
    let res = sites.filter(site => {
        return site.regex.test(atSite);
    });
    console.log("results>>>", res);
    return res.length > 0;
}

function addRegex(sites) {
    let regexps = sites.map(site => {
        site.regex = new RegExp(site.site.replace(/\./g, '\.').replace(/\*/g, '.*') + '$');
        return site;
    });
    return regexps;
}

function getAllowedSites(intentionManifest) {
    let siteList = intentionManifest.intentions
        .flatMap(item => (item.allowed_sites.map(site => ({
            intention: item.name,
            site: site
        }))))
        .flatMap(allowedSite => {
            if (allowedSite.site.startsWith('group:')) {
                let groupName = allowedSite.site.substring(6);
                return intentionManifest.groups[groupName].map(site => ({
                    intention: allowedSite.intention,
                    site: site
                }));
            }
            return [ allowedSite ];
        });

    return addRegex(siteList);
}

function handleResponse(message) {
    console.log('got message', message);
    let allowedSites = getAllowedSites(message);
    console.log("1>", allowedSites);
    let allowed = currentSiteAllowed(allowedSites);
    console.log("2>", allowed);
    if (!allowed) {
        displayNotice(allowedSites);
    }
}

function handleError(err) {
    console.log("Error", err);
}

function allowedSitesHTML(sites) {
    let root = document.createElement("div");
    let topMsg = document.createElement("div");
    topMsg.innerHTML = "The current site is not allowed. Active intentions:";
    root.appendChild(topMsg);
    for (var i = 0; i < sites.length; i++) {
        var elem = document.createElement("p");
        elem.innerHTML = `<span style="font-weight:bold;">${sites[i].intention}</span>: ${sites[i].site}`;
        elem.style.margin = "0";
        elem.style.padding = "0";
        root.appendChild(elem);
    }
    root.style.position = "fixed";
    root.style.left = "0";
    root.style.right = "0";
    root.style.top = "0";
    root.style.bottom = "0";
    root.style.padding = "20px";
    root.style.fontSize = "1.2rem";
    root.style.color = "black";
    root.style.backgroundColor = "white";
    root.style.fontFamily = "Helvetica Neue";
    root.style.textAlign = "left";
    root.style.zIndex = "10000";
    return root;
}

function displayNotice(sites) {
    console.log("block:", sites);
    console.log(allowedSitesHTML(sites));
    document.body.appendChild(allowedSitesHTML(sites));
}

// let siteRegex = /^.*ycombinator.*$/g;
chrome.runtime.sendMessage(null, {"tabId": "test"}, null, handleResponse);
