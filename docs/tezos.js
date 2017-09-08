(function() {
    // Define XHR event handlers.

    function transferComplete(evt) {
        // This should not happen as the response is streamed without end.
        console.log("transferComplete", timeDisplay(), evt);
    }

    function transferCancelled(evt) {
        // This happens when reloading the page.
        console.log("transferCancelled", timeDisplay(), evt);

        window.setTimeout(sendRequest, 5000);
    }

    function transferFailed(evt) {
        // This should not happen, but I've seen it.
        console.log("transferFailed", timeDisplay(), evt);

        // Send the request again to restart the response stream.
        // TODO: Some kind of backoff on repeated failures.
        window.setTimeout(sendRequest, 5000);
    }

    function updateProgress(evt) {
        //console.log("updateProgress", timeDisplay(), evt);

        // Get all the text received since the stream last settled with a valid chunk.
        var new_response = xhr.responseText.substring(lengthParsed);

        // One new chunk in the response stream can contain several JSON
        // units. Try to break them apart and handle separately.
        var units = new_response.split(/}{/);

        for (var i = 0; i < units.length; i++) {
            var unit = units[i];
            // add back the delimiters that we split by
            if (i > 0) {
                unit = "{" + unit;
            }
            if (i < units.length - 1) {
                unit = unit + "}";
            }
            try {
                var parsed = JSON.parse(unit);
            } catch (e) {
                //console.log("parse failure", i, unit.length, e);
                break;
            }
            //console.log("parsed", i, parsed);
            app.ports.monitor.send(parsed);
        }

        //console.log("parsed n of m:", i, units.length);

        // It's possible for one JSON unit to be large enough to require
        // multiple response chunks, so we determine how much of the new input
        // we were able to parse and adjust our progress mark only that far so
        // that the incomplete chunk will be considered when the next chunk
        // arrives.
        var newlyParsedText = units.slice(0, i).join('}{');
        lengthParsed += newlyParsedText.length;
    }


    function sendRequest() {
        try {
            // Send the single request that will stream its response. We will
            // handle the received chunks via "progress" events.
            xhr.open("POST", SERVER + "/blocks", true);
            xhr.setRequestHeader("Content-Type", "application/json");
            xhr.send('{"monitor": true}');
        } catch (e) {
            console.log("XHR exception", timeDisplay(), e);
        }
    }

    var timeFormat = new Intl.DateTimeFormat('en-US', {
        month: 'numeric', day: 'numeric',
        hour: 'numeric', minute: 'numeric',second: 'numeric',
        hour12: false
    });

    function timeDisplay() {
        var date = new Date();
        return timeFormat.format(date);
    }

    // MAIN

    const SERVER = "https://tezos.ostraca.org";

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
    var lengthParsed = 0;

    sendRequest();
})();
