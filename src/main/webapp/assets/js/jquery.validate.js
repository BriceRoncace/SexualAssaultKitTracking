(function($) {
  $.fn.validate = function() {
    this.on('submit', validateForm);
    var formValid = null;
    
    function validateForm(e) {
      if (!formValid) {
        e.preventDefault();
        var $form = $(this);
        formValid = valid($form);
        if (formValid) {
          $form.submit();
        }
      }
    }
    
    function valid($form) {
      var valid = true;
      $form.find("[data-required]:not(:disabled):not(:hidden):not(.tt-hint)").each(function(i, required) {
        var $required = $(required);
        if (!isRequiredValueFound($required)) {
          displayAsInvalid($required);
          valid = false;
        }
        else {
          displayAsValid($required);
        }
      });
      
      return valid;
    }
    
    function isRequiredValueFound($required) {
      if ($required.is(":checkbox")) {
        return $required.is(":checked") ? true : false;
      }
      if ($required.is(":radio")) {
        return $("input[name='" + $required.attr("name") + "']:checked").length === 1 ? true : false;
      }
      
      return $.trim($required.val()) !== '' ? true : false;
    }
    
    function displayAsInvalid($required) {
      $required.addClass("invalid");
      findInParent($required, "label.required").addClass("invalid");
    }
    
    function displayAsValid($required) {
      $required.removeClass("invalid");
      findInParent($required, "label.required").removeClass("invalid");
    }
    
    function findInParent($el, selectorToFind, parentLimit) {
      parentLimit = parentLimit || 3;
      
      var $parent = $el.parent();
      var $found = $parent.find(selectorToFind);
      var x = 0;
      while ($found.length === 0 && x < parentLimit) {
        $parent = $parent.parent();
        $found = $parent.find(selectorToFind);
        x = x + 1;
      }
      return $found;
    }
    
    return this;
  };
}(jQuery));
