var d = new Date();

var flags = {
    nodeUrl: "https://tezos.ostraca.org",
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
            // if we get here then we have a complete JSON data object
            xhr.previous_text = xhr.responseText;
            app.ports.monitor.send(new_response);
        }
        catch (e)
        {
            if (e.name != "SyntaxError") {
                console.log("exception", e);
            }
        }
    }   
}
     
try {
    xhr.open("POST", "https://tezos.ostraca.org/blocks", true);
    xhr.setRequestHeader("Content-Type", "application/json");
    xhr.send('{"monitor": true}');
}
catch (e)
{
    console.log("XHR exception", e);
}
