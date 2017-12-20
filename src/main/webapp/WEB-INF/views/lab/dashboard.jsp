<%@page contentType="text/html" pageEncoding="UTF-8"%>
<%@ taglib prefix="cjisTags" uri="http://isp.idaho.gov/jsp/cjisTags"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="t" tagdir="/WEB-INF/tags" %>
<t:page>
  <jsp:attribute name="head">
    <link href="<c:url value="/assets/css/pagination.css"/>" rel="stylesheet" type="text/css" />
  </jsp:attribute>
  <jsp:attribute name="body">
    <%@include file="../../includes/org-nav.jspf" %>
    <%@include file="../../includes/messages.jspf" %>
    <c:if test="${user.admin}"><t:breadcrumb crumbActive="${organization.name} Dashboard"/></c:if>
    <h2><c:out value="${organization.name}"/> Dashboard</h2>
    
    <!-- Tabs -->
    <ul id="dashboard-tabs" class="nav nav-tabs" role="tablist">
      <li role="presentation"><a href="#newKits" aria-controls="new" role="tab" data-toggle="tab">New Kits <span class="badge">${newKits != null ? newKits.totalElements : 0}</span></a></li>
      <li role="presentation"><a href="#incomingKits" aria-controls="incoming" role="tab" data-toggle="tab">Incoming Kits <span class="badge">${incomingKits != null ? incomingKits.totalElements : 0}</span></a></li>
      <li role="presentation" class="active"><a href="#inProcessKits" aria-controls="inProcess" role="tab" data-toggle="tab">In-Process Kits <span class="badge">${inProcessKits != null ? inProcessKits.totalElements : 0}</span></a></li>
    </ul>

    <!-- Tab panes -->
    <div class="tab-content">
      <div role="tabpanel" class="tab-pane" id="newKits">
        <%@include file="includes/kit-tabs/new.jspf" %>
      </div>
      <div role="tabpanel" class="tab-pane" id="incomingKits">
        <%@include file="includes/kit-tabs/incoming.jspf" %>
      </div>
      <div role="tabpanel" class="tab-pane active" id="inProcessKits">
        <%@include file="includes/kit-tabs/in-process.jspf" %>
      </div>
    </div>
    
  </jsp:attribute>
  <jsp:attribute name="scripts">
    <script src="<c:url value="/assets/js/saktrack.js"/>"></script>
    <script src="<c:url value="/assets/js/jquery.cookie.js"/>"></script>
    <script src="<c:url value="/assets/js/jquery.stickytabs.js"/>"></script>
    <script type="text/javascript">
      $(function() {
        $("#dashboard-tabs").stickytabs();
        
        $("[data-seed-kit]").click(function() {
          var serialNumber = $(this).data("seed-kit");
          var selector = $(this).data("seed-selector");
          $(selector).val(serialNumber);
        });
        
        sakTrack.initTableCheckboxes();
        
        $("[data-receive-kit]").click(function() {
          $("#kitToReceive").val($(this).data("receive-kit"));
          $("#receiveFromSelect").data("default-val", $(this).data("kit-from"));
        });
        
        $(".modal").on('shown.bs.modal', function() {
          $(this).find("select[data-organizations-url]").each(function(i, select){
            var $select = $(select);
            $.getJSON($select.data('organizations-url'), function(orgs) {
              sakTrack.initOrganizationSelect(orgs, $select);
            });
          });
        });
        
        $("#sendKitsModal").on('shown.bs.modal', function() {
          var $kitSerialsInput = $("#serialNumbersToSend");
          if (!$kitSerialsInput.val()) {
            $kitSerialsInput.val(sakTrack.getCheckedSerialNumbers("#newKitsTable"));
          }
        });
        
        $("#deleteKitsModal").on('shown.bs.modal', function() {
          var $kitSerialsInput = $("#deleteSerialNumbers");
          if (!$kitSerialsInput.val()) {
            $kitSerialsInput.val(sakTrack.getCheckedSerialNumbers("#newKitsTable"));
          }
        });
        
        $("#createKitBtn").click(function(e) {
          e.preventDefault();
          var $createKitForm = $("#createKitsForm");
          $.post("<c:url value="/lab/createKitCount"/>", $createKitForm.serialize(), function(count) {
            if (count >= 100) {
              $("#kitCount").html(count);
              $("#createKitConfirmationModal").modal();
            }
            else {
              $createKitForm.submit();
            }
          });
        });
        
        $("#confirmCreateKitBtn").click(function() {
          $("#createKitsForm").submit();
        });
      });
    </script>
  </jsp:attribute>
</t:page>