<%@page contentType="text/html" pageEncoding="UTF-8"%>
<%@ taglib prefix="t" tagdir="/WEB-INF/tags" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>

<t:page>
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
      
    <%@include file="includes/modals/send-kits-modal.jspf" %>
    
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
          $("#serialNumbersToReceive").val($(this).data("receive-kit"));
          $("#receiveFromSelect").data("default-val", $(this).data("kit-from"));
        });
        
        $("[data-receive-kits]").click(function() {
          $("#serialNumbersToReceive").val(sakTrack.getCheckedSerialNumbers("#incomingKitsTable"));
          $("#receiveFromSelect").data("defaultVal", sakTrack.getFirstCheckedFrom("#incomingKitsTable"));
        });
        
        $("[data-send-kit]").click(function() {
          $("#serialNumbersToSend").val($(this).data("send-kit"));
        });
        
        $("[data-send-kits]").click(function() {
          $("#serialNumbersToSend").val(sakTrack.getCheckedSerialNumbers("#inProcessKitsTable"));
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
        
        $(".modal").on('shown.bs.modal', function() {
          sakTrack.initOrganizationSelects($(this));
        });
      });
    </script>
  </jsp:attribute>
</t:page>