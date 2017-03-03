var cl4l = {
    call: function(elemID, eventID, event) {
        var seq = cl4l.pushEvent(event);

        var data = cl4l.updates;
        cl4l.updates = {};
        
        data["cl4l-doc"] = cl4l.docID;
        data["cl4l-elem"] = elemID;
        data["cl4l-event"] = eventID;
        
        $.ajax(cl4l.URL, {
            data: data,
            dataType: "script",
            method: "POST",
            error: function(req, err, ex) {
                cl4l.popEvent(seq, false);
                console.error(req, err, ex);
            },
            success: function() {
                cl4l.popEvent(seq, true);
            }
        });
    },
    docID: null,
    events: {},
    nextSeq: 0,
    onclick: function(elemID, event) {
        if (cl4l.skipEvent()) { return; }
        cl4l.call(elemID, "onclick", event);
    },
    onkeydown: function(elemID, event) {
        if (cl4l.skipEvent()) { return; }
        cl4l.update("cl4l-key", event.which);
        cl4l.update("cl4l-alt-key", event.altKey);
        cl4l.update("cl4l-ctrl-key", event.ctrlKey);
        cl4l.update("cl4l-shift-key", event.shiftKey);
        cl4l.call(elemID, "onkeydown", event);
    },
    skipEvent: function() {
        var res = cl4l._skipEvent;
        cl4l._skipEvent = false;
        return res;
    },
    popEvent: function(seq, call) {
        var ev = cl4l.events[seq];
        if (ev) {
            delete cl4l.events[seq];
            if (call) {
                cl4l._skipEvent = true;
                ev.target.dispatchEvent(ev);
            }
        }
    },
    pushEvent: function(event) {
        event.stopPropagation();
        var seq = cl4l.nextSeq;
        cl4l.nextSeq++;
        cl4l.update("cl4l-seq", seq);
        cl4l.events[seq] = event;
        return seq;
    },
    _skipEvent: false,
    URL: null,
    update: function(id, val) {
        cl4l.updates[id] = val;
    },
    updates: {}
};
