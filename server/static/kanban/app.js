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
        var baseuri = "/users/" +
            encodeURIComponent($("#user").val()) +
            "/tasks?filter=" +
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
                        var info = tag("div", { class: "info" })
                            .hide()
                            .html(simpleFormat(this.description));

                        tag("li", { class: "task" })
                            .append(tag("h2").text(this.name).click(function() {
                                info.slideToggle("fast");
                            }))
                            .append(info)
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