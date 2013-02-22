var elpad = 
    (function () {
         var init_pads = function () {
             if (window["pads"] !== undefined) {
                 $(pads).each (
                     function (i,e) {
                         var li = $("#pad-list").append(
                             "<li><a id='" + e + "' href='javascript:;'>" 
                                 + e + "</a></li>"
                         );
                     });
                 $("#pad-list li a").on(
                     "click",
                     function (evt)  { console.log("id ", evt.target.id); }
                 );
             }
         };

         init_pads();
         return {
             // Functions to expose
         };
     })();