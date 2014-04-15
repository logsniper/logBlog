function leaveMessageForm (parent_msgid, author) {
    $("#leave_a_msg .rpmsgid").remove();
    var form = "<div id=\"leave_a_msg\">" + $("#leave_a_msg").html() + "</div>";
    $("#leave_a_msg").remove();
    if (parent_msgid >= 0) {
        $("#"+parent_msgid).append(form);
        var rplMsgInfo = "<input class=\"rpmsgid\" type=\"hidden\" name=\"rpmsg\" value=" + parent_msgid + " />";
        $("#leave_a_msg form").append(rplMsgInfo);
        $("#leave_a_msg textarea").text("回复" + author + ": ");
    } else {
        $("#non_reply_msg").after(form);
        $("#leave_a_msg textarea").text("");
    }
    $("#leave_a_msg").hide().slideDown();
}

function validateEmail(email) { 
    var re = /^(([^<>()[\]\\.,;:\s@\"]+(\.[^<>()[\]\\.,;:\s@\"]+)*)|(\".+\"))@((\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\])|(([a-zA-Z\-0-9]+\.)+[a-zA-Z]{2,}))$/;
    return re.test(email);
} 

function updateParagraphIDGivenClass(newID, class_name) {
    $("." + class_name).each(function () {
        var cur_arr = $(this).attr("name").split("_");
        var curID = cur_arr[cur_arr.length - 1];
        if (curID >= newID) {
            var new_attr = "";
            for (var i = 0; i < cur_arr.length - 1; ++ i) {
                new_attr += cur_arr[i] + "_";
            }
            ++ curID;
            new_attr += curID;
            $(this).attr("name", new_attr);
        }
    });
}

function updateParagraphAddLink(newID) {
    $(".para_add").each(function() {
        var curID = parseInt($(this).attr("id"));
        if (curID >= newID) {
            curID += 1;
            $(this).attr({"onclick": "insertParagraphIDafter(" + curID + ")", "id": curID});
        }
    });
}

function updateParagraphInsertPos(newID) {
    $(".insert_pos").each(function() {
        var posid = parseInt($(this).attr("id"));
        if (posid >= newID) {
            $(this).attr("id", posid + 1);
        }
    });
}

function insertNewBlock(newID) {
    var blockContent = [
'<tr class=insert_pos id=' + newID + ' ><td>++++++++++</td></tr>',
'    <tr>',
'      <td>',
'        Paragraph: NEW',
'        <input class=para_type type="radio" name="para_type_' + newID + '" value="ptype-head"/> head',
'        <input class=para_type type="radio" name="para_type_' + newID + '" value="ptype-body" checked="true"/> text',
'        <input class=para_type type="radio" name="para_type_' + newID + '" value="ptype-image"/> image',
'      </td>',
'    </tr>',
'    <tr>',
'      <td><textarea class=para_text name="para_text_' + newID + '"></textarea></td>',
'    </tr>',
'<tr><td><a id=' + newID + ' class=para_add href="javascript:void(0)" onclick="insertParagraphIDafter(' + newID + ')">add paragraph</a></td></tr>'
    ].join('\n');
    var find_posid = newID + 1;
    $(".insert_pos#" + find_posid).before(blockContent);
}

function insertParagraphIDafter(lastID) {
    var newID = lastID + 1;
    updateParagraphIDGivenClass(newID, "para_type");
    updateParagraphIDGivenClass(newID, "para_text");
    updateParagraphAddLink(newID);
    updateParagraphInsertPos(newID);
    insertNewBlock(newID);
}

$(function() {
    $("#register_submit").click(function(e) {
        e.preventDefault();
        if ($("#input_email").val() == undefined || validateEmail($("#input_email").val())) {
            $("#input_email_hint").text("");
            $("#register_form").submit();
        } else {
            $("#input_email_hint").text("邮箱地址不合法.INVALID EMAIL ADDRESS.");
        }
    });
});
