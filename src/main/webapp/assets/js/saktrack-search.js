$(function () {
  initDateInputs();
  initButtonGroups();
  $(".clickable.label[data-search-type]").click(searchTypeLabelClickHandler);
  $("[data-btn-val").click(buttonGroupClickHandler);
  $("[data-toggle-trigger]").click(function() {
    $("[data-toggleable]").toggleClass("hidden");
  });

  function initButtonGroups() {
    $(".btn-group > input:hidden").each(function(i, input) {
      var $hiddenInput = $(input);
      highlightSelectedButton($hiddenInput);
    });

    function highlightSelectedButton($hiddenInput) {
      var value = $hiddenInput.val();
      $hiddenInput.siblings("[data-btn-val='" + value +"']").removeClass("btn-default").addClass("btn-primary");
    }
  }

  function initDateInputs() {
    $("input:hidden[name$='searchType']").each(function(i, input) {
      var $hiddenInput = $(input);
      highlightSelectedLabel($hiddenInput);
      showOrHideSecondDate($hiddenInput);
      disableIfNone($hiddenInput);
    });

    function highlightSelectedLabel($hiddenInput) {
      var searchType = $hiddenInput.val();
      $hiddenInput.siblings(".clickable.label[data-search-type!='" + searchType +"']").removeClass("label-primary").addClass("label-default");
      $hiddenInput.siblings(".clickable.label[data-search-type='" + searchType +"']").removeClass("label-default").addClass("label-primary");
    }

    function showOrHideSecondDate($hiddenInput) {
      var $inputGroup = $hiddenInput.siblings(".input-group");
      var searchType = $hiddenInput.val();
      if (showSecondDate(searchType)) {
        $inputGroup.find("input[name$='date2']").removeClass("invisible");
        $inputGroup.find(".input-group-addon").removeClass("invisible");
      }
      else {
        $inputGroup.find("input[name$='date2']").val("").addClass("invisible");
        $inputGroup.find(".input-group-addon").addClass("invisible");
      }
    }

    function showSecondDate(searchType) {
      return searchType === "BETWEEN";
    }
    
    function disableIfNone($hiddenInput) {
      var $inputGroup = $hiddenInput.siblings(".input-group");
      var searchType = $hiddenInput.val();
      if (disableFirstDate(searchType)) {
        $inputGroup.find("input[name$='date1']").attr("disabled", true);
      }
      else {
        $inputGroup.find("input[name$='date1']").attr("disabled", false);
      }
    }
    
    function disableFirstDate(searchType) {
      return searchType === "NONE";
    }
  }

  function buttonGroupClickHandler() {
    var $this = $(this);
    if (isActive($this)) {
      clearAllInGroup($this);
      setButtonGroupValue($this, "");
    }
    else {
      clearAllInGroup($this);      
      activate($this);
      setButtonGroupValue($this, $this.data("btn-val"));
    }

    function isActive($button) {
      return $button.hasClass("btn-primary");
    } 

    function activate($button) {
      return $button.addClass("btn-primary");
    } 

    function clearAllInGroup($button) {
      $button.parent(".btn-group").find(".btn").removeClass("btn-primary").addClass("btn-default");
    }

    function setButtonGroupValue($button, value) {
      $button.siblings("input:hidden").val(value);
    }
  }

  function searchTypeLabelClickHandler() {
    var $this = $(this);
    var searchType = $this.data("searchType");
    $this.siblings("input:hidden").val(searchType);
    initDateInputs();
  }
});