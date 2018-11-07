<%@page contentType="text/html" pageEncoding="UTF-8"%>
<%@ taglib prefix="t" tagdir="/WEB-INF/tags" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>

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
      <li role="presentation"><a href="#incomingKits" aria-controls="incoming" role="tab" data-toggle="tab">Incoming <span class="badge">${incomingKits != null ? incomingKits.totalElements : 0}</span></a></li>
      <li role="presentation" class="active"><a href="#inProcessKits" aria-controls="inProcess" role="tab" data-toggle="tab">Pending Submission <span class="badge">${inProcessKits != null ? inProcessKits.totalElements : 0}</span></a></li>
      <li role="presentation"><a href="#atLabKits" aria-controls="atLabKits" role="tab" data-toggle="tab">At Lab <span class="badge">${kitsInLab != null ? kitsInLab.totalElements : 0}</span></a></li>
      <li role="presentation"><a href="#analyzedKits" aria-controls="analyzed" role="tab" data-toggle="tab">Analyzed <span class="badge">${analyzedKits != null ? analyzedKits.totalElements : 0}</span></a></li>
      <li role="presentation"><a href="#unsubmittableKits" aria-controls="unsubmittable" role="tab" data-toggle="tab">Not Meeting Criteria <span class="badge">${unsubmittableKits != null ? unsubmittableKits.totalElements : 0}</span></a></li>
    </ul>
    
    <!-- Tab panes -->
    <div class="tab-content">
      <div role="tabpanel" class="tab-pane" id="incomingKits">
        <%@include file="includes/kit-tabs/incoming.jspf" %>
      </div>
      <div role="tabpanel" class="tab-pane active" id="inProcessKits">
        <%@include file="includes/kit-tabs/in-process.jspf" %>
      </div>
      <div role="tabpanel" class="tab-pane" id="atLabKits">
        <%@include file="includes/kit-tabs/at-lab.jspf" %>
      </div>
      <div role="tabpanel" class="tab-pane" id="analyzedKits">
        <%@include file="includes/kit-tabs/analyzed.jspf" %>
      </div>
      <div role="tabpanel" class="tab-pane" id="unsubmittableKits">
        <%@include file="includes/kit-tabs/unsubmittable.jspf" %>
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
        $('[data-toggle="tooltip"]').tooltip();
        
        sakTrack.initTableCheckboxes();
        
        $("[data-seed-kit]").click(function() {
          var serialNumber = $(this).data("seed-kit");
          var selector = $(this).data("seed-selector");
          $(selector).val(serialNumber);
        });
        
        $("[data-receive-kit]").click(function() {
          $("#serialNumbersToReceive").val($(this).data("receive-kit")).prop("readonly", true);
          var fromId = $(this).data("kit-from");
          
          $.getJSON( "<c:url value="/organizations?type=LAB&type=MEDICAL&type=LAW_ENFORCEMENT"/>", function(orgs) {
            sakTrack.initOrganizationSelect(orgs, "#receiveFromSelect");
            $("#receiveFromSelect").val(fromId);
          });
        });
        
        $("[data-receive-kits]").click(function() {
          var $kitSerialsInput = $("#serialNumbersToReceive").prop("readonly", false);
          if (!$kitSerialsInput.val()) {
            $kitSerialsInput.val(sakTrack.getCheckedSerialNumbers("#incomingKitsTable"));
          }
          
          var fromId = sakTrack.getFirstCheckedFrom("#incomingKitsTable");
          $.getJSON( "<c:url value="/organizations?type=LAB&type=MEDICAL&type=LAW_ENFORCEMENT"/>", function(orgs) {
            sakTrack.initOrganizationSelect(orgs, "#receiveFromSelect");
            $("#receiveFromSelect").val(fromId);
          });
        });
        
        $(".modal").on('shown.bs.modal', function() {
          $(this).find("select[data-organizations-url]").each(function(i, select){
            var $select = $(select);
            $.getJSON($select.data('organizations-url'), function(orgs) {
              sakTrack.initOrganizationSelect(orgs, $select);
            });
          });
        });
      });
    </script>
  </jsp:attribute>
</t:page>