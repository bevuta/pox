var tag = function(tag, attrs) {
    var el = $("<" + tag + "/>");
    if (attrs) el.attr(attrs);
    return el;
};

$(function() {
    var controls = tag("div", { id: "controls" });
    var columns  = tag("tr");
    
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