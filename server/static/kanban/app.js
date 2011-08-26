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

        $.each(text.split("\n\n"), function(i, body) {
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
                .appendTo(col)
                .focus(); 

            tag("div", { class: "tasks" })
                .appendTo(col);
        };
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
                        tag("li", { class: "task" })
                            .append(tag("h2").text(this.name))
                            .append(tag("div", { class: "description" })
                                    .html(simpleFormat(this.description)))
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
    });

})();