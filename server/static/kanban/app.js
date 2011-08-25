var tag = function(tag, attrs) {
    var el = $("<" + tag + "/>");
    if (attrs) el.attr(attrs);
    return el;
};

$(function() {
    var controls = tag("div", { id: "controls" });
    var columns  = tag("tr");
    var user     = tag("input", { id: "user" })
        .appendTo(controls)
        .before(tag("label", { for: "user" }).text("User"));
    var load   = tag("button").text("Load").appendTo(controls);
    
    $("body")
        .append(controls)
        .append(tag("table", { id: "columns" })
                .append(columns));

    tag("button")
        .text("Add column")
        .click(function() {
            var col = tag("td")
                .text("a column")
                .addClass("column");
            columns.append(col);
        })
        .appendTo(controls);
});