/*! Lazy Load 1.9.3 - MIT license - Copyright 2010-2013 Mika Tuupola */
!function(a,b,c,d){var e=a(b);a.fn.lazyload=function(f){function g(){var b=0;i.each(function(){var c=a(this);if(!j.skip_invisible||c.is(":visible"))if(a.abovethetop(this,j)||a.leftofbegin(this,j));else if(a.belowthefold(this,j)||a.rightoffold(this,j)){if(++b>j.failure_limit)return!1}else c.trigger("appear"),b=0})}var h,i=this,j={threshold:0,failure_limit:0,event:"scroll",effect:"show",container:b,data_attribute:"original",skip_invisible:!0,appear:null,load:null,placeholder:"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAAAXNSR0IArs4c6QAAAARnQU1BAACxjwv8YQUAAAAJcEhZcwAADsQAAA7EAZUrDhsAAAANSURBVBhXYzh8+PB/AAffA0nNPuCLAAAAAElFTkSuQmCC"};return f&&(d!==f.failurelimit&&(f.failure_limit=f.failurelimit,delete f.failurelimit),d!==f.effectspeed&&(f.effect_speed=f.effectspeed,delete f.effectspeed),a.extend(j,f)),h=j.container===d||j.container===b?e:a(j.container),0===j.event.indexOf("scroll")&&h.bind(j.event,function(){return g()}),this.each(function(){var b=this,c=a(b);b.loaded=!1,(c.attr("src")===d||c.attr("src")===!1)&&c.is("img")&&c.attr("src",j.placeholder),c.one("appear",function(){if(!this.loaded){if(j.appear){var d=i.length;j.appear.call(b,d,j)}a("<img />").bind("load",function(){var d=c.attr("data-"+j.data_attribute);c.hide(),c.is("img")?c.attr("src",d):c.css("background-image","url('"+d+"')"),c[j.effect](j.effect_speed),b.loaded=!0;var e=a.grep(i,function(a){return!a.loaded});if(i=a(e),j.load){var f=i.length;j.load.call(b,f,j)}}).attr("src",c.attr("data-"+j.data_attribute))}}),0!==j.event.indexOf("scroll")&&c.bind(j.event,function(){b.loaded||c.trigger("appear")})}),e.bind("resize",function(){g()}),/(?:iphone|ipod|ipad).*os 5/gi.test(navigator.appVersion)&&e.bind("pageshow",function(b){b.originalEvent&&b.originalEvent.persisted&&i.each(function(){a(this).trigger("appear")})}),a(c).ready(function(){g()}),this},a.belowthefold=function(c,f){var g;return g=f.container===d||f.container===b?(b.innerHeight?b.innerHeight:e.height())+e.scrollTop():a(f.container).offset().top+a(f.container).height(),g<=a(c).offset().top-f.threshold},a.rightoffold=function(c,f){var g;return g=f.container===d||f.container===b?e.width()+e.scrollLeft():a(f.container).offset().left+a(f.container).width(),g<=a(c).offset().left-f.threshold},a.abovethetop=function(c,f){var g;return g=f.container===d||f.container===b?e.scrollTop():a(f.container).offset().top,g>=a(c).offset().top+f.threshold+a(c).height()},a.leftofbegin=function(c,f){var g;return g=f.container===d||f.container===b?e.scrollLeft():a(f.container).offset().left,g>=a(c).offset().left+f.threshold+a(c).width()},a.inviewport=function(b,c){return!(a.rightoffold(b,c)||a.leftofbegin(b,c)||a.belowthefold(b,c)||a.abovethetop(b,c))},a.extend(a.expr[":"],{"below-the-fold":function(b){return a.belowthefold(b,{threshold:0})},"above-the-top":function(b){return!a.belowthefold(b,{threshold:0})},"right-of-screen":function(b){return a.rightoffold(b,{threshold:0})},"left-of-screen":function(b){return!a.rightoffold(b,{threshold:0})},"in-viewport":function(b){return a.inviewport(b,{threshold:0})},"above-the-fold":function(b){return!a.belowthefold(b,{threshold:0})},"right-of-fold":function(b){return a.rightoffold(b,{threshold:0})},"left-of-fold":function(b){return!a.rightoffold(b,{threshold:0})}})}(jQuery,window,document);
/* lazyload plugin ends*/

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
$(function () {
    $("img.photo").lazyload({
        threshold : 200,
        effect : "fadeIn"
    });
});

/* Google Analytics begin */
(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
(i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
})(window,document,'script','//www.google-analytics.com/analytics.js','ga');

ga('create', 'UA-50077939-1', 'logsniper.com');
ga('send', 'pageview');
/* Google Analytics end */
