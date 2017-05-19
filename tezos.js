(function () {

    var d = new Date();

    var flags = {
        nodeUrl: "http://localhost:8732",
        now: d.getTime()
    };

    var app = Elm.Main.fullscreen(flags);

    var xhr = new XMLHttpRequest();

    xhr.previous_text = '';

    xhr.onerror = function() { console.log("fatal XHR error"); };

    xhr.onreadystatechange = function()
    {
        var t = new Date();
        var timestamp = t.getMinutes() + ":" + t.getSeconds();

        if (xhr.readyState > 2)
        {
            var new_response = xhr.responseText.substring(xhr.previous_text.length);
            try {
                var result = JSON.parse( new_response );
                // If we get here (because no exception happened) then we have a
                // complete JSON data object. We take that to mean that the
                // stream has settled and we have the next chunk that we can
                // deliver to the app.
                xhr.previous_text = xhr.responseText;
                app.ports.monitor.send(result);
            }
            catch (e)
            {
                if (e.name != "SyntaxError") {
                    console.log("exception", e);
                }
                // else new_response is not well-formed JSON; wait for more and try again
            }
        }   
    }
    
    try {
        // Send the single request that will stream its response. We will handle
        // the received chunks via onreadystatechange above.
        xhr.open("POST", "http://localhost:8732/blocks", true);
        xhr.setRequestHeader("Content-Type", "application/json");
        xhr.send('{"monitor": true}');
    }
    catch (e)
    {
        console.log("XHR exception", e);
    }

})();
