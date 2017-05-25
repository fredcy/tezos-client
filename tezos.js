(function() {
    // Define XHR event handlers.

    function transferComplete(evt) {
        // This should not happen as the response is streamed without end.
        console.log("transferComplete", evt);
    }

    function transferCancelled(evt) {
        // This happens when reloading the page.
        console.log("transferCancelled", evt);
    }

    function transferFailed(evt) {
        // This should not happen, but I've seen it.
        console.log("transferFailed", evt);

        // Send the request again to restart the response stream.
        // TODO: Some kind of backoff on repeated failures.
        setTimeout(sendRequest, 5000);
    }


    function timeDisplay() {
        var date = new Date();
        return date.getHours() + ":" + date.getMinutes() + ":" + date.getSeconds();
    }

    function updateProgress(evt) {
        console.log("updateProgress", timeDisplay(), evt);

        // Get all the text received since the stream last settled with a valid chunk.
        var new_response = xhr.responseText.substring(previous_text.length);

        try {
            // Try to parse the chunk received so far.
            var result = JSON.parse(new_response);

            // If we get here (because no exception happened) then we have a
            // complete JSON data object. We take that to mean that the stream
            // has settled and we have the next chunk that we can deliver to the
            // app.
            previous_text = xhr.responseText;
            app.ports.monitor.send(result);
        } catch (e) {
            if (e.name != "SyntaxError") {
                console.log("JSON.parse exception", e);
            }
            // else new_response is not well-formed JSON; wait for more and try again
        }
    }


    function sendRequest() {
        try {
            // Send the single request that will stream its response. We will
            // handle the received chunks via "progress" events.
            xhr.open("POST", SERVER + "/blocks", true);
            xhr.setRequestHeader("Content-Type", "application/json");
            xhr.send('{"monitor": true}');
        } catch (e) {
            console.log("XHR exception", e);
        }
    }

    // MAIN

    const SERVER = "http://localhost:8732";

    var flags = {
        nodeUrl: SERVER,
        now: (new Date()).getTime()
    };

    var app = Elm.Main.fullscreen(flags);

    var xhr = new XMLHttpRequest();

    xhr.addEventListener("error", transferFailed);
    xhr.addEventListener("progress", updateProgress);
    xhr.addEventListener("load", transferComplete);
    xhr.addEventListener("abort", transferCancelled);

    // Keep state on stream received so far, up through latest complete chunk.
    var previous_text = "";

    sendRequest();
})();
