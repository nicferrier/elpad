var padapp = 
    (function () {

         var send_change = function (evt, pos) {
             $.post(
                 "/change/",
                 { "change": evt.which, "pos": pos },
                 function (data, textStatus) {
                 }
             );
         };

         var change = function (evt) {
             if (evt.type == "keypress") {
                 send_change(evt, $("#text")[0].selectionStart + 1);
             }
             else if (evt.type == "keydown" && evt.which == 8) {
                 console.log("keydown", evt);
                 send_change(evt, $("#text")[0].selectionStart + 1);
             }
         };

         $(document).ready(
             function () {
                 $("#text").on("keypress keydown", change);
                 $.getJSON("/pad/", 
                           function (data) {
                               $("#text").val(data.text);
                           });
             }
         );
         
         return {
             change: change
         };
     })();
