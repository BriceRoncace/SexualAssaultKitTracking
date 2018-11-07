<%@ tag trimDirectiveWhitespaces="true" %>
<%@ tag description="Page Layout" pageEncoding="UTF-8"%>
<%@ attribute name="title" fragment="true" %>
<%@ attribute name="head" fragment="true" %>
<%@ attribute name="css" fragment="true" %>
<%@ attribute name="body" fragment="true" %>
<%@ attribute name="scripts" fragment="true" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>

<!DOCTYPE html>
<html>
  <head>
    <!-- current page: ${pageContext.request.requestURI} -->
    <meta charset="utf-8">
    <meta http-equiv="X-UA-Compatible" content="IE=edge">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <title><c:choose><c:when test="${title == null}">Sexual Assault Kit Tracking</c:when><c:otherwise><jsp:invoke fragment="title" /></c:otherwise></c:choose></title>
    <script src="https://ajax.googleapis.com/ajax/libs/jquery/1.12.4/jquery.min.js"></script>
    <jsp:invoke fragment="head" />
    <link href="https://maxcdn.bootstrapcdn.com/bootswatch/3.3.7/yeti/bootstrap.min.css" rel="stylesheet" integrity="sha384-HzUaiJdCTIY/RL2vDPRGdEQHHahjzwoJJzGUkYjHVzTwXFQ2QN/nVgX7tzoMW3Ov" crossorigin="anonymous">
    <link href="https://maxcdn.bootstrapcdn.com/font-awesome/4.6.3/css/font-awesome.min.css" rel="stylesheet" integrity="sha384-T8Gy5hrqNKT+hzMclPo118YTQO6cYprQmhrYwIiQ/3axmI1hQomh7Ud2hPOy8SP1" crossorigin="anonymous">
    <link href="<c:url value="/assets/css/bootstrap-datetimepicker.min.css"/>" rel="stylesheet" type="text/css" />
    <link href="<c:url value="/assets/css/saktrack.css"/>" rel="stylesheet" type="text/css" />
    <jsp:invoke fragment="css" />
  </head>
  <body data-context-path="${pageContext.request.contextPath}">
    <div class="container">
      <jsp:invoke fragment="body" />
    </div>
    
    <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js" integrity="sha384-Tc5IQib027qvyjSMfHjOMaLkfuWVxZxUPnCJA7l2mCWNIpG9mGCD8wGNIcPD7Txa" crossorigin="anonymous"></script>
    <script src="<c:url value="/assets/js/bootstrap-datetimepicker.min.js"/>" ></script>
    <script src="<c:url value="/assets/js/jquery.confirmation.js"/>"></script>
    <script src="<c:url value="/assets/js/jquery.tablesorter.min.js"/>"></script>
    <script src="<c:url value="/assets/js/jquery.validate.js"/>"></script>
    <script src="<c:url value="/assets/js/jquery.dynamic-action.js"/>"></script>
    <script type="text/javascript">
      $(function () {
        $(document).confirmation();
        $(document).on('init-datetime', initDateTimePickers);
        $(document).on('click', '[data-href]', goToLink);
        $(document).on('click', '[data-show]', show);
        $(document).on('click', '[data-hide]', hide);
        
        $("form").validate().dynamicAction();
        
        $(".modal").on('shown.bs.modal', function() {
          $("[data-modal-focus]", this).focus();
        }).on('hidden.bs.modal', function() {
          $("[data-modal-clear]", this).val("");
        });
        
        initDateTimePickers();
        initFocus();

        function initFocus() {
          $("[data-focus]").focus();
          focusIfEmpty($("[data-focus-if-empty]"));
          
          function focusIfEmpty($el) {
            if ($el.val() === '') {
              $el.focus();
            }
          }
        }

        function initDateTimePickers() {
          var dateTimePicker = {format: 'mm/dd/yyyy HH:ii P', autoclose: true, forceParse: false, showMeridian: true};
          var datePicker = {format: 'mm/dd/yyyy', autoclose: true, todayHighlight: true, minView: 2, forceParse: false};
          var timePicker = {format: "HH:ii P", autoclose: true, startView: 1, forceParse: false, showMeridian: true};
          
          $('.hasDateTimePicker').datetimepicker(dateTimePicker);
          $('.hasDatePicker').datetimepicker(datePicker);
          $('.hasTimePicker').datetimepicker(timePicker).on('show', function (e) {
            $(this).datetimepicker('setValue', e.date);
          });
        }
        
        var tableSorterOptions = {cssNoSort: 'noSort', widgets: ['saveSort']};
        initTableSorter();

        function initTableSorter() {
          $('.sortableTable').tablesorter(tableSorterOptions);
        }
        
        function goToLink() {
          location.href = $(this).attr('data-href');
        }
        
        function show() {
          var $this = $(this);
          $($this.attr('data-show')).removeClass("hidden").find("select, input").addBack('select, input').prop('disabled', false);
          $this.trigger("cjis:show");
        }
        
        function hide() {
          var $this = $(this);
          $($this.attr('data-hide')).addClass("hidden").find("select, input").addBack('select, input').prop('disabled', true);
          $this.trigger("cjis:hide");
        }
      });
    </script>
    <jsp:invoke fragment="scripts" />
  </body>
</html>