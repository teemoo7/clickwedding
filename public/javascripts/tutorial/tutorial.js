function startTutorial() {
    showStep(0);
}

function showStep(step) {
    $(".mark").hide();

    var content = steps[step];
    var dialogTitle = title + " - " + (1+step) + " / " + steps.length;
    $("#tutorialDialogMessage").html(content);
    $("#tutorialDialogMessage *[data-target]").click(function(event) {
        var target = $('#'+$(this).attr('data-target'));
        $.scrollTo(target, {duration:500});
        target.effect("highlight", {}, 3000);
        event.preventDefault();
    });

    $("#tutorialDialogMessage *[data-target]").each(function(index, value) {
        var target = $('#'+$(this).attr('data-target'));
        var id = "mark_"+index;
        var $div = $('<div class="mark" style="display: none;"></div>').appendTo('body');
        $div.attr('id', id);
        var pos = $(target).offset();
        var width = $(target).width();
        var height = $(target).height();
        var leftEdit = (pos.left+(width-159)/2);
        var topEdit = (pos.top+(height-74)/2);
        $div.css({top: topEdit, left: leftEdit});
        $div.show();
    });

    var buttons = [];
    if (step > 0) {
        buttons[buttons.length] = { text: previousButton, click: function() { showStep(step-1); } };
    }
    if (steps.length > step+1) {
        buttons[buttons.length] = { text: nextButton, click: function() { showStep(step+1); } };
    } else {
        buttons[buttons.length] = { text: closeButton, click: function() { $( this ).dialog( "close" ); } };
    }
    $("#tutorialDialog").dialog({
        modal: true,
        buttons: buttons,
        width: 500,
        title: dialogTitle,
        close: endTutorial
    });
}

function endTutorial() {
    $(".mark").hide();
}

$(function() {
    startTutorial();
});