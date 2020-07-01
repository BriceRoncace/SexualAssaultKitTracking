var sakTrack = (function() {
  
  function initOrganizationSelects($el) {
    $el.find("select[data-organizations-url]").each(function(i, select){
      var $select = $(select);
      $.getJSON($select.data('organizations-url'), function(orgs) {
        initOrganizationSelect(orgs, $select);
        $select.val($select.data("defaultVal"));
      });
    });
  }
  
  function initOrganizationSelect(organizations, selector) {
    var $select = $(selector).html("");
    $select.append($("<option></option>")); 
    $.each(organizations, function(i, organization) {   
      $select.append($("<option/>", { value: organization.id, text: organization.name})); 
    });
    if ($select.data("default-val")) {
      $select.val($select.data("default-val"));
    }  
  }
  
  function initTableCheckboxes() {
    $("input:checkbox[data-header-checkbox]").click(function() {
      var $headerCheckbox = $(this);
      if ($headerCheckbox.is(":checked")) {
        $headerCheckbox.closest("table").find("input:checkbox[data-kit-serial]").prop('checked', true);
      }
      else {
        $headerCheckbox.closest("table").find("input:checkbox[data-kit-serial]").prop('checked', false);
      }
    });
  }
  
  function getCheckedSerialNumbers(selector) {
    var checkedKits = [];
    $(selector).find("input:checkbox[data-kit-serial]").each(function(i, checkbox) {
      var $cb = $(checkbox);
      if ($cb.is(":checked")) {
        checkedKits.push($cb.data("kit-serial"));
      }
    });
    return checkedKits ? checkedKits.join("\n") : "";
  }
  
  function getFirstCheckedFrom(selector) {
    var $checkboxes = $(selector).find("input:checkbox[data-kit-from]:checked"); 
    return $checkboxes.length > 0 ? $($checkboxes[0]).data("kit-from") : "";
  }
  
  var publicApi = {};
  publicApi.initOrganizationSelects = initOrganizationSelects;
  publicApi.initOrganizationSelect = initOrganizationSelect;
  publicApi.initTableCheckboxes = initTableCheckboxes;
  publicApi.getCheckedSerialNumbers = getCheckedSerialNumbers;
  publicApi.getFirstCheckedFrom = getFirstCheckedFrom;
  return publicApi;
})();