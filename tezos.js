(function() {
    // MAIN

    const SERVER = "http://localhost:8732";

    var flags = {
        nodeUrl: SERVER,
        now: (new Date()).getTime()
    };

    var app = Elm.Main.fullscreen(flags);
})();
