<%@page contentType="text/html" pageEncoding="UTF-8"%>
<%@ taglib prefix="t" tagdir="/WEB-INF/tags" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>

<t:page>
  <jsp:attribute name="body">
    <%@include file="../../includes/org-nav.jspf" %>
    <%@include file="../../includes/messages.jspf" %>
    <c:if test="${user.admin}"><t:breadcrumb crumbActive="${organization.name} Dashboard"/></c:if>
    <h2 class="bottom15 top0"><c:out value="${organization.name}"/> Dashboard</h2>
    
    <!-- Tabs -->
    <ul id="dashboard-tabs" class="nav nav-tabs" role="tablist">
      <li role="presentation"><a href="#incomingKits" aria-controls="incoming" role="tab" data-toggle="tab">Incoming Kits <span class="badge">${incomingKits != null ? incomingKits.totalElements : 0}</span></a></a></li>
      <li role="presentation" class="active"><a href="#inProcessKits" aria-controls="inProcess" role="tab" data-toggle="tab">In-Process Kits <span class="badge">${inProcessKits != null ? inProcessKits.totalElements : 0}</span></a></a></li>
    </ul>

    <!-- Tab panes -->
    <div class="tab-content">
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
         
        sakTrack.initTableCheckboxes();
        
        $("[data-receive-kit]").click(function() {
          $("#serialNumbersToReceive").val($(this).data("receive-kit"));
          $("#receiveFromSelect").data("defaultVal", $(this).data("kit-from"));
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
        
        $(".modal").on('shown.bs.modal', function() {
          sakTrack.initOrganizationSelects($(this));
        });
        
      });
    </script>
  </jsp:attribute>
</t:page>
