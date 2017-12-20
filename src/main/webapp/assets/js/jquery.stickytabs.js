(function($) {
  $.fn.stickytabs = function() {
    var $el = this;
    var id = $el.attr("id");
    var key = "sticky-tab-" + id ? id : getRandomInt(1000,2000);
   
    $el.find('a[data-toggle="tab"]').click(saveTabInCookie);
    showTabBasedOnCookie();
   
    function getRandomInt(min, max) {
      return Math.floor(Math.random() * (max - min + 1)) + min;
    }
   
    function saveTabInCookie() {
      $.cookie(key, $(this).attr("href"));
    }
   
    function showTabBasedOnCookie() {
      var lastTab = $.cookie(key);
      if (lastTab) {
        $("a[href='" + lastTab + "']").tab('show');
      }
    };

    return this;
  };
}(jQuery));
