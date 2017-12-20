(function ($) {
  $.fn.confirmation = function() {
    var defaultLabel = "Are you sure?";
    var defaultText = "This action may be irreversible. Are you sure you would like to continue?";
    var isSubmitHandlerActive = true;

    function addConfirmationModalToDom() {
      var confirmDiv = $('<div></div>').attr("id", "confirm").addClass("modal fade").attr("tabindex", "-1").attr("role", "dialog").data("backdrop", "static");

      var dialog = $("<div></div>").addClass("modal-dialog");
      var content = $("<div></div>").addClass("modal-content");

      var header = $('<div></div>').addClass("modal-header");
      var headerCloseBtn = $("<button></button>").attr("type", "button").addClass("close dismiss-confirmation").append("&times;");
      var headerLabel = $("<h4></h4>").attr("id", "confirmationLabel").append($("<span></span>").attr("id", "confirmationLabel"));
      header.append(headerCloseBtn).append(headerLabel);

      var modalBody = $("<div></div>").addClass("modal-body").append($("<p></p>").append($("<span></span>").attr("id", "confirmationText")));

      var modalFooter = $("<div></div>").addClass("modal-footer");
      var footerHiddenHref = $("<input></input>").attr("type", "hidden").attr("id", "confirm-href");
      var footerYesBtn = $("<button></button>").addClass("btn btn-danger").attr("id", "yesConfirm").html("Yes");
      var footerNoBtn = $("<button></button>").addClass("btn btn-default dismiss-confirmation").attr("id", "noCancel").html("No");

      modalFooter.append(footerHiddenHref).append(footerYesBtn).append(footerNoBtn);

      content.append(header).append(modalBody).append(modalFooter);
      dialog.append(content);
      confirmDiv.append(dialog);

      $("body").append(confirmDiv);
    }

    function submittingText() {
      $("#yesConfirm").hide();
      $("#noCancel").hide();
      $("#confirmationLabel").html("Processing...");
      $(".modal-body #confirmationText").html("Processing...");
    }

    dismiss = function() {
      $("#confirm").modal('hide');
    };

    confirmSubmit = function(e) {
      if (!isSubmitHandlerActive) {
        return;
      }
      e.preventDefault();
      var confirmLabel = $(this).data('confirmation-label') || defaultLabel;
      $("#confirmationLabel").html(confirmLabel);
      var confirmText = $(this).data('confirmation-text') || defaultText;
      $(".modal-body #confirmationText").html(confirmText);
      $("#confirm").modal('show');
      var $form = $(this).is('form') ? $(this) : $(this).closest('form');

      $("#yesConfirm").off().on("click", function() {
        submittingText();
        isSubmitHandlerActive = false;
        $form.submit();
      });
    };

    addConfirmationModalToDom();
    this.on("click", ".confirmation", confirmSubmit);
    $("#confirm").on("click", ".dismiss-confirmation", dismiss);

    return this;
  };
})(jQuery);