(function () {

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

    var intersperse = function(array, sep) {
        if (!array.length) return array;
        
        var result = [array[0]];
        
        $.each(array.slice(1), function(i, e) {
            result.push(sep);
            result.push(e)
        });

        return result;
    };

    var simpleFormat = function(text) {
        var result = $("<div/>");

        $.each((text || "").split("\n\n"), function(i, body) {
            var p = tag("p");

            $.each(intersperse($.trim(body).split("\n"), $("<br/>")), function(i, el) {
                if (typeof el == "string") {
                    p.append($("<div/>").text(el).text());
                } else {
                    p.append(el);
                }
            });

            result.append(p);
        });

        return result.html();
    };

    var addColumnTo = function(columns) {
        return function() {
            var col = tag("td", { class: "column" }).appendTo(columns);

            col.droppable({ 
                accept: ".task",
                drop: function(event, ui) {
                    // TODO: ajax update request!
                    ui.draggable.fadeOut().delay(1000).fadeIn();
                }
            });


            tag("input", { class: "tag" })
                .appendTo(tag("div", { class: "wrap" }).appendTo(col))
                .keyup(function(ev) {
                    if (ev.which == 13) {
                        loadTasks();
                    }
                })
                .focus();

            tag("div", { class: "tasks" })
                .appendTo(col);

            return col;
        };
    };

    var addColumnsFromQueryString = function(columns) {
        var add = addColumnTo(columns);

        $.each(($.query.get("columns") || "").split(" "), function(i, col) {
            if (col.length) {
                add().find(".tag").val(col);
            }
        });
    };

    var loadTasks = function() {
        var baseuri = "/tasks?filter=" +
            encodeURIComponent($("#filter").val());

        $(".column").each(function() {
            var col = $(this);
            var coltag = $.trim(col.find(".tag").val());
            var uri = baseuri;

            if (coltag.length) {
                uri += encodeURIComponent(" :" + coltag);
            }

            $.ajax({
                type: "GET",
                dataType: "json",
                url: uri,
                success: function(tasks) {
                    var container = tag("ul");
 
                    $.each(tasks, function() {
                        var details = tag("div", { class: "details" })
                            .hide()
                            .html(simpleFormat(this.description));

                        tag("div", { class: "meta" })
                            .append(tag("span", { class: "id-rev" })
                                    .text("#" + this.id + ":" + this.revision))
                            .append(" ")
                            .append(tag("span", { class: "assigner" })
                                    .text("< " + this.assigner))
                            .prependTo(details);

                        var bar = tag("div", { class: "bar" })
                            .text(this.name)
                            .prepend(" ")
                            .prepend(tag("a", { class: "action" })
                                     .text("[e]")
                                     .click(function() {
                                         details.slideToggle("fast");
                                     }));
                        
                        var meta = tag("div", { class: "meta" })
                            .prependTo(bar);

                        if (this.priority != 0) {
                            tag("span", { class: "priority" })
                                .text((this.priority > 0 ? '+' : '') + this.priority)
                                .appendTo(meta);

                            meta.append(" ");
                        }

                        tag("span", { class: "assignee" })
                            .text(this.assignee)
                            .appendTo(meta);

                        tag("li", { class: "task" })
                            .draggable({ revert: true, cancel: ".action"  })
                            .append(bar)
                            .append(details)
                            .appendTo(container);
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
        var filter   = input("filter", "Filter", controls).val($.query.get("filter"));

        tag("button")
            .text("Add column")
            .click(addColumnTo(columns))
            .appendTo(controls);

        tag("button")
            .text("Load")
            .appendTo(controls)
            .click(loadTasks);


        $("body")
            .append(controls)
            .append(tag("table", { id: "columns" })
                    .append(columns));

        addColumnsFromQueryString(columns);
        loadTasks();
    });

})();