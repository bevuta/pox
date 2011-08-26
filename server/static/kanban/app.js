var tag = function(tag, attrs) {
    var el = $("<" + tag + "/>");
    if (attrs) el.attr(attrs);
    return el;
};

var input = function(id, label, target) {
    var el = tag("input", { id: id });
    el.appendTo(target);
    el.before(tag("label", { for: id }).text(label));
    return el;
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
                var container = tag("ul");

                $.each(tasks, function() {
                    tag("li").text(this.name).appendTo(container);
                });

                col.find(".tasks").html(container);
            }
        });
    });
};

$(function() {
    var controls = tag("div", { id: "controls" });
    var columns  = tag("tr");
    var user     = input("user", "User", controls).val($.query.get("user"));
    var gfilter  = input("gfilter", "Filter", controls).val($.query.get("filter"));

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