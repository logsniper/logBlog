/*! Lazy Load 1.9.3 - MIT license - Copyright 2010-2013 Mika Tuupola */
!function(a,b,c,d){var e=a(b);a.fn.lazyload=function(f){function g(){var b=0;i.each(function(){var c=a(this);if(!j.skip_invisible||c.is(":visible"))if(a.abovethetop(this,j)||a.leftofbegin(this,j));else if(a.belowthefold(this,j)||a.rightoffold(this,j)){if(++b>j.failure_limit)return!1}else c.trigger("appear"),b=0})}var h,i=this,j={threshold:0,failure_limit:0,event:"scroll",effect:"show",container:b,data_attribute:"original",skip_invisible:!0,appear:null,load:null,placeholder:"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAAAXNSR0IArs4c6QAAAARnQU1BAACxjwv8YQUAAAAJcEhZcwAADsQAAA7EAZUrDhsAAAANSURBVBhXYzh8+PB/AAffA0nNPuCLAAAAAElFTkSuQmCC"};return f&&(d!==f.failurelimit&&(f.failure_limit=f.failurelimit,delete f.failurelimit),d!==f.effectspeed&&(f.effect_speed=f.effectspeed,delete f.effectspeed),a.extend(j,f)),h=j.container===d||j.container===b?e:a(j.container),0===j.event.indexOf("scroll")&&h.bind(j.event,function(){return g()}),this.each(function(){var b=this,c=a(b);b.loaded=!1,(c.attr("src")===d||c.attr("src")===!1)&&c.is("img")&&c.attr("src",j.placeholder),c.one("appear",function(){if(!this.loaded){if(j.appear){var d=i.length;j.appear.call(b,d,j)}a("<img />").bind("load",function(){var d=c.attr("data-"+j.data_attribute);c.hide(),c.is("img")?c.attr("src",d):c.css("background-image","url('"+d+"')"),c[j.effect](j.effect_speed),b.loaded=!0;var e=a.grep(i,function(a){return!a.loaded});if(i=a(e),j.load){var f=i.length;j.load.call(b,f,j)}}).attr("src",c.attr("data-"+j.data_attribute))}}),0!==j.event.indexOf("scroll")&&c.bind(j.event,function(){b.loaded||c.trigger("appear")})}),e.bind("resize",function(){g()}),/(?:iphone|ipod|ipad).*os 5/gi.test(navigator.appVersion)&&e.bind("pageshow",function(b){b.originalEvent&&b.originalEvent.persisted&&i.each(function(){a(this).trigger("appear")})}),a(c).ready(function(){g()}),this},a.belowthefold=function(c,f){var g;return g=f.container===d||f.container===b?(b.innerHeight?b.innerHeight:e.height())+e.scrollTop():a(f.container).offset().top+a(f.container).height(),g<=a(c).offset().top-f.threshold},a.rightoffold=function(c,f){var g;return g=f.container===d||f.container===b?e.width()+e.scrollLeft():a(f.container).offset().left+a(f.container).width(),g<=a(c).offset().left-f.threshold},a.abovethetop=function(c,f){var g;return g=f.container===d||f.container===b?e.scrollTop():a(f.container).offset().top,g>=a(c).offset().top+f.threshold+a(c).height()},a.leftofbegin=function(c,f){var g;return g=f.container===d||f.container===b?e.scrollLeft():a(f.container).offset().left,g>=a(c).offset().left+f.threshold+a(c).width()},a.inviewport=function(b,c){return!(a.rightoffold(b,c)||a.leftofbegin(b,c)||a.belowthefold(b,c)||a.abovethetop(b,c))},a.extend(a.expr[":"],{"below-the-fold":function(b){return a.belowthefold(b,{threshold:0})},"above-the-top":function(b){return!a.belowthefold(b,{threshold:0})},"right-of-screen":function(b){return a.rightoffold(b,{threshold:0})},"left-of-screen":function(b){return!a.rightoffold(b,{threshold:0})},"in-viewport":function(b){return a.inviewport(b,{threshold:0})},"above-the-fold":function(b){return!a.belowthefold(b,{threshold:0})},"right-of-fold":function(b){return a.rightoffold(b,{threshold:0})},"left-of-fold":function(b){return!a.rightoffold(b,{threshold:0})}})}(jQuery,window,document);
/* lazyload plugin ends*/

function leaveMessageForm (parent_msgid, author) {
    $("#leave_a_msg #input_rpmsg").remove();
    var form = "<div id=leave_a_msg>" + $("#leave_a_msg").html() + "</div>";
    $("#leave_a_msg").remove();
    if (parent_msgid >= 0) {
        $(".reply_msg#m"+parent_msgid).before(form);
        var rplMsgInfo = '<input id="input_rpmsg" type="hidden" name="rpmsg" value=' + parent_msgid + ' />';
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
        var L = cur_arr.length;
        var curID = cur_arr[L - 1];
        if (curID >= newID) {
            var new_attr = "";
            for (var i = 0; i < L - 1; ++ i) {
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
        var curID = parseInt($(this).attr("sid"));
        if (curID >= newID) {
            curID += 1;
            $(this).attr({"onclick": "insertParagraphIDafter(" + curID + ")", "id": curID, "sid": curID});
        }
    });
}

function updateParagraphInsertPos(newID) {
    $(".insert_pos").each(function() {
        var posid = parseInt($(this).attr("sid"));
        if (posid >= newID) {
            posid += 1;
            $(this).attr({"id": "p" + posid, "sid": posid});
        }
    });
}

function insertNewBlock(newID) {
    var blockContent = '' +
'<tr class=insert_pos id=p' + newID + ' sid=' + newID + '><td>++++++++++</td></tr>' +
'    <tr>' +
'      <td>' +
'        Paragraph: NEW, TYPE: ' +
'        <select class=para_type name="para_type_' + newID + '">' +
'        <option value="ptype-head">head</option>' +
'        <option value="ptype-body" selected="selected">body</option>' +
'        <option value="ptype-image">image</option>' +
'        </select>' +
'      </td>' +
'    </tr>' +
'    <tr>' +
'      <td><textarea class=para_text name="para_text_' + newID + '"></textarea></td>' +
'    </tr>' +
'<tr><td><a id=p' + newID + ' class=para_add sid=' + newID + ' href="javascript:void(0)" onclick="insertParagraphIDafter(' + newID + ')">add paragraph</a></td></tr>';
    var find_posid = newID + 1;
    $(".insert_pos#p" + find_posid).before(blockContent);
}

function insertParagraphIDafter(lastID) {
    var newID = lastID + 1;
    updateParagraphIDGivenClass(newID, "para_type");
    updateParagraphIDGivenClass(newID, "para_text");
    updateParagraphAddLink(newID);
    updateParagraphInsertPos(newID);
    insertNewBlock(newID);
}

function jumpBack() {
    if (document.referrer == "") {
        location.href = '/index';
    } else {
        location.href = document.referrer;
    }
}

function ajaxLogin(e) {
    e.preventDefault();
    var email = $("#login_email").val();
    var psw = $("#login_psw").val();
    if (email != "" && psw != "") {
        //var htmlobj = $.ajax({url:"/ajax_login", async:false, type:"POST", data:{"email":email, "password":psw}});
        var postData = {"email":email, "password":psw};
        $.post("ajax_login", postData, function (responseData, stat) {
            var hintinfo = "";
            var jump = false;
            if (stat == "success") {
                var json = JSON.parse(responseData);
                if (json.status == "11") {
                    $("#login_form").hide();
                    hintinfo = "登陆成功, 即将跳转到前一页面";
                    jump = true;
                } else if (json.status == "12") {
                    hintinfo = "邮箱、密码错误";
                } else if (json.status == "13") {
                    hintinfo = "您的邮箱未注册"
                }
            } else {
                hintinfo = "由于内部原因登录失败";
            }
            if (jump) {
                $("#login_hint").html(hintinfo);
                jumpBack();
            } else {
                $("#login_hint").hide().html(hintinfo).slideDown();
            }
        });
    } else {
        $("#login_hint").hide().text("邮箱或密码不能为空.").slideDown();
    }
}

function cancel_unread(msgid) {
    $.get("cancel_unread?msgid=" + msgid, function (responseData, stat) {
        if (stat == "success") {
            location.href = $("a.cancel_unread#m"+msgid).attr('nexturl');
        }
    });
}

function ajaxSubmitMessage() {
    var postData;
    var content = $("#msg_content").val();
    var blogid = $("#input_blogid").val();
    var rpmsgid = -1;
    var hierarchy = 1;
    var doPost = true;
    if ($("#input_rpmsg").val() != undefined) {
        rpmsgid = $("#input_rpmsg").val();
        // hierarchy只分奇偶，如果前一条是odd，则当前为even，取值2；否则为1
        hierarchy = $(".message#m" + rpmsgid).attr('hierarchy') == "odd" ? 2 : 1; 
    }
    if ($("#input_email").val() == undefined) {
        if (rpmsgid == -1) {
            postData = {'blogid':blogid, 'hierarchy': hierarchy, 'content': content};
        } else {
            postData = {'blogid':blogid, 'hierarchy': hierarchy, 'content': content, 'rpmsg': rpmsgid};
        }
    } else {
        var email = $("#input_email").val();
        var psw = $("#input_psw").val();
        var author = $("#input_author").val();
        if (email == "" || psw == "" || author == "") {
            $("#leave_msg_hint").text("邮箱、昵称和密码不能为空.");
            doPost = false;
        } else if (!validateEmail(email)) {
            $("#leave_msg_hint").text("邮箱地址不合法.INVALID EMAIL ADDRESS.");
            doPost = false;
        } else {
            if (rpmsgid == -1) {
                postData = {'blogid':blogid, 'hierarchy': hierarchy, 'content': content,
                    'email': email, 'password': psw, 'author': author};
            } else {
                postData = {'blogid':blogid, 'hierarchy': hierarchy, 'content': content, 'rpmsg': rpmsgid, 
                    'email': email, 'password': psw, 'author': author};
            }
        }
    }
    if (doPost) {
        $.post("ajax_submit_message", postData, function (responseData, stat) {
            if (stat == "success") {
                if (rpmsgid == -1) {
                    $(".insert_msg_pos#new").after(responseData);
                } else {
                    $(".insert_msg_pos#m" + rpmsgid).after(responseData);
                }
                $("#leave_a_msg").hide();
            } else {
                $("#leave_msg_hint").text("系统内部错误.");
            }
        });
    }
}

function mark_all_unread () {
    $.get("mark_all_unread", function (responseData, stat) {
        if (stat == "success") {
            var json = JSON.parse(responseData);
            $("#mark_response_hint").text("标记成功.");
            $(".message.unread").remove();
            $("#unread_hint").hide();
        }
    });
}

function getURLParameter (sParam) {
    var sPageURL = window.location.search.substring(1);
    var sURLVariables = sPageURL.split('&');
    var L = sURLVariables.length;
    for (var i = 0; i < L; i++) 
    {
        var sParameterName = sURLVariables[i].split('=');
        if (sParameterName[0] == sParam) 
        {
            return sParameterName[1];
        }
    }
    return undefined;
}

function countDown(selector, startValue, callback) {
    var obj = $(selector);
    if (selector != "" && obj.html() == undefined) return;
    var intervalID;
    obj.text(startValue);
    var repeatable = function () {
        -- startValue;
        if (startValue > 0) {
            obj.text(startValue);
        } else {
            clearInterval(intervalID);
            callback();
        }
    }
    intervalID = setInterval(repeatable, 1000);
}

function triggerLazyLoad () {
    $("img.photo").lazyload({
        threshold : 200,
        effect : "fadeIn"
    });
}

function ajaxFetchBlog(blogid) {
    var animationSpeed = 500;
    $.post('/blog_maininfo?blogid=' + blogid, function (responseData, stat) {
        if (stat == 'success') {
            $('#maininfo').fadeOut(animationSpeed);
            $('#msginfo').fadeOut(animationSpeed);
            setTimeout(function(){
                $('#maininfo').html(responseData).fadeIn(animationSpeed);
                $(window).scrollTop($("body").offset().top);
                triggerLazyLoad();
                $.post('/blog_msginfo?blogid=' + blogid, function (responseData, stat) {
                    if (stat == 'success') {
                        $('#msginfo').html(responseData).fadeIn(animationSpeed);
                    }
                });
            }, animationSpeed);
        }
    });
}

function checkUpdate () {
   var latestMsgID = -1;
    if ($(".comments .recent .cancel_unread").get(0) != undefined) {
        // attr返回对象数组中第一个元素的属性
        latestMsgID = parseInt($(".comments .recent .cancel_unread").attr("mid"));
    }
    var dataPost = {'latest_msg_id': latestMsgID};
    $.post("check_update", dataPost, function (responseData, stat) {
        if (stat == "success") {
            var json = JSON.parse(responseData);
            $("#unread_num").text(json.unread_num);
            if (json.unread_num > 0) {
                $("#unread_hint").show();
            } else {
                $("#unread_hint").hide();
            }
            if (json.has_new_msg > 0) {
                $.get("ajax_recent_msg", function (responseData, stat) {
                    if (stat == "success") {
                        $(".comments .recent").hide().html(responseData).fadeIn();
                    }
                });
            }
            if (json.active_user_num != undefined) {
                $("#active_user_num").text(json.active_user_num);
            }
        }
    });
};

$(document).ready(function() {
    $(".register#input_author").blur(function(){
        if ($(this).val() == "") {
            $("#input_author_hint").text("昵称不能为空.");
        } else {
            var dataPost = {"author": $(this).val()}
            $.post("check_author", dataPost, function (responseData, stat) {
                if (stat == "success") {
                    var json = JSON.parse(responseData);
                    var hintinfo = "";
                    if (json.exist == 1) {
                        hintinfo = "该昵称已存在.";
                    }
                    $("#input_author_hint").text(hintinfo);
                }
            });
        }
    });
    $(".register#input_email").blur(function(){
        if ($(this).val() == "") {
            $("#input_email_hint").text("邮箱不能为空.");
        } else {
            var dataPost = {"email": $(this).val()}
            $.post("check_email", dataPost, function (responseData, stat) {
                if (stat == "success") {
                    var json = JSON.parse(responseData);
                    var hintinfo = "";
                    if (json.exist == 1) {
                        hintinfo = "该邮箱已注册.";
                    }
                    $("#input_email_hint").text(hintinfo);
                }
            });
        }
    });
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

$(document).ready(function () {triggerLazyLoad();});

$(document).ready(function () {
    $("#login_submit").click(ajaxLogin);
});

$(document).ready(function () {
    $("#mark_all_unread_button").click(mark_all_unread);
});


$(document).ready(function () {
    if (parseInt($("#unread_num").text()) == 0) {
        $("#unread_hint").hide();
    }
    setInterval(checkUpdate, 15000);
});

/* highlight given message */
$(document).ready(function () {
    $(".message").each(function () {
        if (getURLParameter("hlm") == $(this).attr('id')) {
            $(this).children('.hlzone').css('background', '#fff2a8');
        }
    });
});

$(document).ready(function () {
    countDown(".countdown3", 3, jumpBack);
});

$(document).ready(function () {
    function mouseEnterMenuLayer1Closure (type) {
        return function () {
            if ($(".menu_layer_1").attr("expanded") > 0) {
                $(".menu_layer_2").hide();
                $(".menu_layer_2#" + type).slideDown("fast");
            }
        };
    }
    $(".menu_layer_1 #by_tag").mouseenter(mouseEnterMenuLayer1Closure("by_tag"));
    $(".menu_layer_1 #by_month").mouseenter(mouseEnterMenuLayer1Closure("by_month"));
    $("#menu_button").mouseenter(function () {
        $(".menu_layer_2").hide();
        $(".menu_layer_1").slideDown("fast");
        $(".menu_layer_1").attr("expanded", 1);
    });
    $(document).click(function (e) {
        if (e.target.id != "menu_button" && e.target.id != "by_tag" && e.target.id != "by_month") {
            $(".menu_layer_1").attr("expanded", 0);
            $(".menu_layer_2").hide();
            $(".menu_layer_1").slideUp("fast");
        }
    });
    $("#navigator .nav_button").mouseenter(function (e) {
        if (e.target.id != "menu_button") {
            $(".menu_layer_1").attr("expanded", 0);
            $(".menu_layer_2").hide();
            $(".menu_layer_1").slideUp("fast");
        }
    });
});

$(document).ready(function () {
    var unclick2click = function () {
        $("#backToTopUnclick").hide();
        $("#backToTopClick").show();
    };
    var click2unclick = function () {
        $("#backToTopClick").hide();
        $("#backToTopUnclick").show();
    };
    var goTop = function () {
        unclick2click();
        $('body,html').animate({scrollTop:0},500);
    };
    $("#backToTopUnclick").mouseenter(unclick2click);
    $("#backToTopUnclick").mouseup(goTop);
    $("#backToTopClick").mouseleave(click2unclick);
    $("#backToTopClick").mousedown(click2unclick);
});

$(document).ready(function () {
    $(".online_number").click(function () {
        var showList = $(".online_user_list");
        if (showList.css("display") == "none") {
            checkUpdate();
            $.get("online_users", function (responseData, stat) {
                if (stat == "success") {
                    var response = JSON.parse(responseData);
                    showList.hide();
                    showList.empty();
                    response.users.forEach(function (user) {
                        showList.append('<li>'+user+'</li>');
                    });
                    if (response.tourists) showList.append('<li>游客: '+response.tourists+'</li>');
                    showList.slideDown();
                }
            });
        } else {
            showList.slideUp();
        }
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
