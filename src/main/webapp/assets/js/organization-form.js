$(function () {
  var form = {
    removeUser: function () {
      $('#removeUserForm').submit();
    },
    saveOrg: function () {
      $('#saveOrgForm').submit();
    }
  };

  if ($('#passkeyInput').val() === '') {
    getNewPasskey();
  }
  
  $('#changePasskey').click(getNewPasskey);
  $('[data-remove-user]').click(removeUser);
  $('#saveOrgBtn').click(saveOrganization);
  
  $('[data-submit-form]').click(function () {
    $('.toggle-me').toggleClass('hidden');
    form[$(this).attr('data-submit-form')]();
  });
  
  function saveOrganization() {
    if ($('#origPasskey').val() === "" || $('#passkeyInput').val() === $('#origPasskey').val()) {
      form.saveOrg();
    }
    else {
      $('#confirmAdminActionMessage').html('<p>The passkey has been changed, are you sure you want to do this?</p><p>New passkey: ' + $('#passkeyInput').val() + '</p>');
      $('[data-submit-form]').attr('data-submit-form', 'saveOrg');
      $('#confirmAdminActionDialog').modal('show');
    }
  };

  function removeUser() {
    $('#removeUserId').val($(this).attr('data-remove-user'));
    $('#confirmAdminActionMessage').text('Are you sure you would like to remove ' + $(this).attr('data-remove-user-name') + '?');
    $('[data-submit-form]').attr('data-submit-form', 'removeUser');
    $('#confirmAdminActionDialog').modal('show');
  }

  function getNewPasskey() {
    var contextPath = $("[data-context-path]").data("context-path");
    $.ajax(contextPath + '/organization/passkey').done(function (key) {
      $('#passkeyInput').val(key);
    });
  }
});