// specialFunctions is a map of prefix to function that should return
// a boolean if the site is allowed.
let originalContent = document.body.textContent;
let searchRegexps = [new RegExp("https://duckduckgo.com/*"), new RegExp("https://www.google.com/*")];
let youtubeRegexp = new RegExp("^https://www.youtube.com/$");

let specialFunctions = {
    "search": function(s) {
        let atSite = window.location.href;
        if (searchRegexps.filter(rx => {
            return rx.test(atSite);
        }).length > 0) {
            return true;
        }

        let textRegex = new RegExp(s , "i");

        return textRegex.test(originalContent);
    },
    "youtube": function(s) {
        let atSite = window.location.href;
        if (youtubeRegexp.test(atSite)) {
            return true;
        }
        let textRegex = new RegExp(s, "i");

        if (atSite.includes("results")) {
            let contents = document.getElementById("contents");
            if (textRegex.test(contents ? contents.textContent : "")) {
                return true;
            }
        }

        let titles = document.getElementsByClassName("title");

        for (var i = 0; i < titles.length; i++) {
            if (textRegex.test(titles[i].textContent)) {
                return true;
            }
        }

        let description = document.getElementById("description");
        if (textRegex.test(description ? description.textContent : "")) {
            return true;
        }
        return false;
    }
};

let specialFunctionRegex = /^([a-z]+):(.*)$/;

function currentSiteAllowed(sites) {
    let atSite = window.location.href;
    let res = sites.filter(site => {
        let match = specialFunctionRegex.exec(site.site);
        if (match != null) {
            let sf = specialFunctions[match[1]];
            if (sf) {
                return sf(match[2]);
            }
        }
        return site.regex.test(atSite);
    });
    return res.length > 0;
}

function addRegex(sites) {
    let regexps = sites.map(site => {
        site.regex = new RegExp(site.site.replace(/\./g, '\.').replace(/\*/g, '.*').replace(/\?/g, '\\?').replace(/\+/g, '\\+') + '$');
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
    window.setTimeout(function() {browser.runtime.sendMessage({"tabId": "test"}).then(handleResponse, handleError);}, 2000);

    let allowedSites = getAllowedSites(message);
    let allowed = currentSiteAllowed(allowedSites);

    if (!allowed) {
        displayNotice(allowedSites);
    } else {
        hideNotice();
    }
}

function handleError(err) {
    console.log("Error", err);
}

var screenID = "intentional";

function allowedSitesHTML(root, sites) {
    let topMsg = document.createElement("div");
    topMsg.innerHTML = "The current site is not allowed. Active intentions:";
    root.appendChild(topMsg);
    for (var i = 0; i < sites.length; i++) {
        var elem = document.createElement("p");
        var title = document.createElement("span");
        title.style.fontWeight = "bold";
        title.innerText = sites[i].intention + ": ";
        var site = document.createElement("span");
        site.innerText = sites[i].site;

        elem.appendChild(title);
        elem.appendChild(site);

        elem.style.margin = "0";
        elem.style.padding = "0";
        root.appendChild(elem);
    }
    root.style.overflowY = "scroll";
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

function hideNotice() {
    let screen = document.getElementById(screenID);
    if (!screen) {
        return;
    }
    screen.style.display = "none";
}

function displayNotice(sites) {
    let root = document.getElementById(screenID);
    if (root) {
        root.style.display = "";
        root.innerHTML = "";
        allowedSitesHTML(root, sites);
    } else {
        originalContent = document.body.textContent;
        root = document.createElement("div");
        root.id = screenID;
        document.body.appendChild(allowedSitesHTML(root, sites));
    }
}

browser.runtime.sendMessage({}).then(handleResponse, handleError);
