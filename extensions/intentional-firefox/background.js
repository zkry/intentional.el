browser.runtime.onMessage.addListener(notify);

var port = browser.runtime.connectNative("intentional");

var data = {
    "groups": {
        "reading": []
    },
    "intentions": []
}

port.onMessage.addListener((response) => {
    data = response;
});

port.postMessage("");

function notify(message, sender, sendResponse) {
    sendResponse(data);
}
