(function($) {
  $.fn.dynamicAction = function() {
    this.find('[data-action]').click(changeFormAction);
    
    function changeFormAction() {
      $el = $(this);
      var $form = $el.closest('form');
      $form.attr('action', $el.data('action'));
    }
    
    return this;
  };
}(jQuery));