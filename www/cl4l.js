var cl4l = {
    call: function(elemID, event) {
        var data = cl4l.updates;
        cl4l.updates = {};
        
        data["cl4l-doc"] = cl4l.docID;
        data["cl4l-elem"] = elemID;
        data["cl4l-event"] = event;
        
        $.ajax(cl4l.URL, {
            data: data,
            dataType: "script",
            method: "POST",
            error: function(req, err, ex) {
                console.error(req, err, ex);
            }
        });
    },
    docID: null,
    URL: null,
    update: function(id, val) {
        cl4l.updates[id] = val;
    },
    updates: {}
};
