$(function() {
  var help = $("<div/>").attr("id", "help")
    .append($("<div/>").attr("class", "content").html("\
  <h2>Help</h2> \
  <h3>Task Line Syntax</h3> \
  <pre>* [description] (#([id]:[revision])? [meta-data])?</pre> \
  Examples: \
  <dl> \
    <dt>A new task for yourself:</dt> \
    <dd><pre>* do something</pre></dd> \
    <dt>An existing task with the ID 274, revision 2:</dt> \
    <dd><pre>* make stuff #274:2</pre></dd> \
  </dl> \
  \
 "));

  function div(id) {
    return $("<div/>").attr({ id: id }).appendTo($("body"));
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

  function handleRequestError(request, status) {
    showMessage(request.statusText, status);
  }

  function save() {
    tasks = $("#tasks");

    $.ajax({
      url: tasksURI(tasks.data("user")),
      type: "POST",
      data: tasks.find("textarea").val(),
      contentType: "text/x-downtime",
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
      .append($("<input/>").attr({ type: "checkbox", id: "include-done", name: "include-done", value: $.query.get("include-done") }).keyup(refresh))
      .append($("<label/>").attr({ "for": "include-done" }).text(" include done tasks "))
      .append(button("Show", "show-tasks").click(refresh))
      .append(button("?", "show-help").click(showHelp));

    $("#user").get(0).focus();
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