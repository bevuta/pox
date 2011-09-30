$(function() {
  var help = $("<div/>").attr("id", "help")
        .append($("<div/>").attr("class", "content").html(
[
"",
"<h2>Help</h2>",
"<h3>Syntax</h3>",
"<pre>task     = &quot;*&quot; &lt;name&gt; &lt;meta&gt;? (&quot;\\n&quot; &lt;description&gt;)? ",
"name     = [^#\\n]+",
"meta     = &quot;#&quot; (&lt;id&gt; (&quot;:&quot; &lt;revision&gt;)?)? &lt;property&gt;*",
"id       = [[:digit:]]+",
"revision = [[:digit:]]+",
"priority = [[:digit:]] ; default: 0",
"property = &quot;&gt;&quot; &lt;assignee&gt; | &quot;&lt;&quot; &lt;assigner&gt; | ",
"           [-+] &lt;priority&gt; | &quot;:&quot; &lt;tag&gt;",
"grouping = &quot;#&quot;+ &lt;property&gt;*</pre>",
"<h3>Keys</h3>",
"<dl>",
"<dt>Ctrl+Alt+S</dt>",
"<dd>Save contents of editing area. On success the current view is refreshed. Conflicts are displayed in the editing area to be resolved.</dd></dl>",
"<h3>Task Examples</h3>",
"<dl>",
"<dt>A new task for yourself:</dt>",
"<dd>",
"<pre>* do something</pre></dd>",
"<dt>An existing task with the ID 274, revision 2:</dt>",
"<dd>",
"<pre>* make stuff #274:2</pre></dd>",
"<dt>Delegate task to user frank:</dt>",
"<dd>",
"<pre>* fix it # &gt; frank</pre></dd>",
"<dt>A task with priority 9 assigned to you by joe:</dt>",
"<dd>",
"<pre>* add frob feature # +9 &lt; joe</pre></dd></dl>",
"<h3>Grouping</h3>",
"<p>Tasks can be grouped by preceding them with a &lt;grouping&gt;",
"line. Groupings can be nested, nesting levels are indicated by the",
"amount of leading &quot;#&quot; characters. Groupings cascade their properties",
"to all tasks below their level.</p>",
"<h3>Grouping Examples</h3>",
"<dl>",
"<dt>Two tasks for frank:</dt>",
"<dd>",
"<pre># &gt; frank",
"* do this # +2",
"* do that</pre></dd>",
"<dt>Priorities are assigned from the nearest grouping:</dt>",
"<dd>",
"<pre># +9 :bug",
"* this is priority +9 and tagged 'bug'",
"## -5",
"* this is priority -5 and also tagged 'bug'",
"* this is priority +1 and also tagged 'bug' # +1</pre></dd>",
"<dt>Tags are cascaded additively:</dt>",
"<dd>",
"<pre># :bug",
"* this is an important bug # :important",
"## :critical",
"* this is a critical bug</pre></dd></dl>",
].join("\n")
        ));

    var tag = function(tag, attrs) {
        var el = $("<" + tag + "/>");
        if (attrs) el.attr(attrs);
        return el;
    };

    function div(id) {
        return tag("div", { id: id }).appendTo($("body"));
    }

    function button(label, id) {
        return $("<button/>").attr({ type: "button", id: id }).text(label);
    }

    function refresh() {
        var user = $("input[name=user]").val();
        $.cookie('user', user, { expires: 10000 });

        if (user && !user.match(/^\s+$/)) {
            showTasks(user, 
		              $("#group-by").val(), 
		              $("#filter").val(),
		              $("#include-done").is(":checked"));
        }
    }

    function refreshOnReturn(e) {
        if (e.keyCode == "13") {
            refresh();
        }
    };

    function handleRequestError(response, status) {
        if (response.status == 401) {
            showLogin();
        } else {
            showMessage(response.responseText, status);
        }
    }

    function save() {
        tasks = $("#tasks");

        $.ajax({
            url: tasksURI(tasks.data("user")),
            type: "POST",
            processData: false,
            data: tasks.find("textarea").val(),
            contentType: "text/x-downtime",
            beforeSend: function(request) {
	        request.setRequestHeader("Accept", "text/x-downtime");
            },
            error: function(request, status) {
	            handleRequestError(request, status);

	            if (request.status == 409) {
	                tasks.find("textarea").val(request.responseText);
	            }
            },
            success: function() {
	            showMessage("Saved!", "success");
	            refresh();
            }
        });
    }

    function tasksURI(user) {
        return "/users/" + encodeURIComponent(user) + "/tasks";
    }

    function showMessage(text, type) {
        var message = $("<p/>").addClass(type).text(text).appendTo($("#messages"));
        setTimeout(function() { message.fadeOut("fast", function() { message.remove() }); }, 1000);
    }

    var showHelp = (function() {
        var originalWidth = null
        
        return function() {
            var textarea = $("#tasks textarea");

            if (help.is(":visible")) {
                textarea.width(originalWidth);
                help.hide();
            } else {
                originalWidth = textarea.width();
                textarea.width("58%");
                help.width("40%");
                help.show();
            }
        }
    })();

    function showControls() {
        div("controls")
            .append($("<label/>").attr({ "for": "user" }).text("User: "))
            .append($("<input/>").attr({ type: "text", id: "user", name: "user", value: $.query.get("user") || $.cookie('user') }).keyup(refreshOnReturn))
            .append($("<label/>").attr({ "for": "group-by" }).text(" Group by:  "))
            .append($("<input/>").attr({ type: "text", id: "group-by", name: "group-by", value: $.query.get("group-by") }).keyup(refreshOnReturn))
            .append($("<label/>").attr({ "for": "group-by" }).text(" Filter:  "))
            .append($("<input/>").attr({ type: "text", id: "filter", name: "filter", value: $.query.get("filter") }).keyup(refresh))
            .append($("<input/>").attr({ type: "checkbox", id: "include-done", name: "include-done", value: $.query.get("include-done") }).change(refresh))
            .append($("<label/>").attr({ "for": "include-done" }).text(" include done tasks "))
            .append(button("Show", "show-tasks").click(refresh))
            .append(button("?", "show-help").click(showHelp));

        refresh();
    }

    function showTasks(user, groups, filter, includeDone) {
        var data = { "omit-origin": true };

        if (groups && !groups.match(/^\s*$/)) {
            data["group-by"] = groups;
        }

        if (filter && !filter.match(/^\s*$/)) {
            data["filter"] = filter;
        }

        if (includeDone) {
            data["include-done"] = includeDone;
        }

        $.ajax({
            url: tasksURI(user), 
            type: "GET",
            data: data,
            beforeSend: function(request) {
	        request.setRequestHeader("Accept", "text/x-downtime");
            },
            success: function(data) {
	            $("#tasks").data("user", user).find("textarea").val(data); 
            },
            error: handleRequestError
        });
    }

    var shadow = {
        show: function(content) {
            var shadow = $("#shadow");

            if (!shadow.length) {
                shadow = tag("div", { id: "shadow" })
                    .hide()
                    .css({
                        background: "black",
                        position: "absolute",
                        left: 0,
                        top: 0,
                        right: 0,
                        bottom: 0
                    })
                    .appendTo($("body"));
            }

            shadow
                .after(content.fadeIn())
                .fadeTo("fast", 0.7);
        },

        hide: function() {
            $("#shadow").fadeOut("fast");
        }
    };

    function showLogin() {
        if ($("#login-screen").length) return;
        
        var screen = tag("div", { id: "login-screen" })
            .css({ 
                position: "fixed",
                right: "10em",
                top: "2em"
            })
            .append(tag("form")
                    .append(tag("div", { class: "element" })
                            .append(tag("label", { for: "login-user" }).text("User"))
                            .append(tag("input", { id: "login-user", type: "text", value: $("#user").val() })))
                    .append(tag("div", { class: "element" })
                            .append(tag("label", { for: "login-password" }).text("Password"))
                            .append(tag("input", { id: "login-password", type: "password" })))
                    .append(tag("div", { class: "button" })
                            .append(tag("input", { type: "submit", value: "login" })))
                    .submit(function() {
                        var user = $("#login-user").val();
                        
                        $.ajax({ 
                            url: "/session",
                            type: "POST",
                            data: {
                                user: user, 
                                password: $("#login-password").val()
                            },
                            success: function(body) {
                                $("#user").val(user);
                                shadow.hide();
                                $("#login-screen").remove();
                            },
                            error: function(response, status) {
                                alert(response.responseText);
                            }
                        });

                        return false;
                    }));

        shadow.show(screen);

        if ($("#login-user").val().length) {
            $("#login-password").get(0).focus();
        } else {
            $("#login-user").get(0).focus();
        }
    }

    div("messages");
    $("body").append($("<h1/>").text("pox"));
    showControls();
    div("tasks").append($("<textarea/>")).append(help);

    $(document).keyup(function(event) {
        if (event.ctrlKey && event.altKey && event.keyCode == 83) {
            save();
        }
    });
});