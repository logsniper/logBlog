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
