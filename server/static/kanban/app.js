var tag = function(tag, attrs) {
    var el = $("<" + tag + "/>");
    if (attrs) el.attr(attrs);
    return el;
};

var input = function(id, label) {
    return tag("input", { id: id })
        .before(tag("label", { for: id }).text(label));
};

var loadTasks = function() {
    var baseuri = "/users/" +
        encodeURIComponent($("#user").val()) +
        "/tasks?filter=" +
        encodeURIComponent($("#gfilter").val());

    $(".column").each(function() {
        var col = $(this);
        var filter = col.find(".filter").val();
        var uri = baseuri;

        if (filter.length) {
            uri += encodeURIComponent(" " + filter);
        }

        $.ajax({
            type: "GET",
            dataType: "json",
            url: uri,
            success: function(tasks) {
                col.find(".tasks")
                    .text(tasks);
            }
        });
    });
};

$(function() {
    var controls = tag("div", { id: "controls" });
    var columns  = tag("tr");
    var user     = input("user", "User").appendTo(controls);
    var gfilter  = input("gfilter", "Filter").appendTo(controls);

    tag("button")
        .text("Add column")
        .click(function() {
            var col = tag("td", { class: "column" }).appendTo(columns);

            tag("input", { class: "filter" })
                .appendTo(col)
                .focus(); 

            tag("div", { class: "tasks" }).
                appendTo(col);
        })
        .appendTo(controls);

    tag("button")
        .text("Load")
        .appendTo(controls)
        .click(loadTasks);

    $("body")
        .append(controls)
        .append(tag("table", { id: "columns" })
                .append(columns));
});