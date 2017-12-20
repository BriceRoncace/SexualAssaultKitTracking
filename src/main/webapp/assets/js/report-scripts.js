$(function () {
  $('[data-clickable]').click(function () {
    $('.' + $(this).attr('data-clickable')).toggleClass('hidden');
    var $span = $('#' + $(this).attr('data-clickable'));
    if ($span.hasClass("glyphicon-menu-down")) {
      $span.removeClass("glyphicon-menu-down");
      $span.addClass("glyphicon-menu-up");
    }
    else {
      $span.removeClass("glyphicon-menu-up");
      $span.addClass("glyphicon-menu-down");
    }
  });
  
  $('#show-all').click(function () {
    $('.kitRows').removeClass('hidden');
  });
  
  $('#hide-all').click(function () {
    $('.kitRows').addClass('hidden');
  });
});