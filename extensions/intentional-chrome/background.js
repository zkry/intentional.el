chrome.runtime.onMessage.addListener(notify);

var port = chrome.runtime.connectNative("com.zkry.intentional");

var data = {
    "groups": {
        "reading": []
    },
    "intentions": []
};

port.onMessage.addListener((response) => {
    data = response;
});

port.postMessage("");

function notify(message, sender, sendResponse) {
    sendResponse(data);
}
