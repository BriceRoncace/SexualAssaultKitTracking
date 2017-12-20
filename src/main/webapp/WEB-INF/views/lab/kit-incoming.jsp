<%@page contentType="text/html" pageEncoding="UTF-8"%>
<%@ taglib prefix="cjisTags" uri="http://isp.idaho.gov/jsp/cjisTags"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="t" tagdir="/WEB-INF/tags" %>
<t:page>
  <jsp:attribute name="body">
    <%@include file="../../includes/org-nav.jspf" %>
    <%@include file="../../includes/messages.jspf" %>
    <t:breadcrumb crumbActive="Kit #${kit.serialNumber}"/>
    <h2 class="line-under">Sexual Assault Kit #${kit.serialNumber}
      <div class="pull-right">
        <a class="btn btn-xs btn-default" href="<c:url value="/timeline"/>?serialNumber=${kit.serialNumber}"><i class="fa fa-clock-o font14" aria-hidden="true"></i> Kit Timeline</a>
        <button type="button" class="btn btn-xs btn-default" data-toggle="modal" data-receive-kit="${kit.serialNumber}" data-kit-from="${kit.currentCustody.from.id}" data-target="#receiveKitModal"><span class="glyphicon glyphicon-arrow-down"></span> Receive Kit...</button>
      </div>
    </h2>
    <p>
      Kit #${kit.serialNumber} sent from <c:out value="${kit.currentCustody.from}"/> on <cjisTags:fmtTemporal value="${kit.currentCustody.eventDate}" pattern="${dateFormat}" /> by ${kit.currentCustody.actor}<br/>
    </p>
    
    <%@include file="includes/modals/receive-kit-modal.jspf" %>
  </jsp:attribute>
  <jsp:attribute name="scripts">
    <script src="<c:url value="/assets/js/saktrack.js"/>"></script>
    <script type="text/javascript">
      $(function() {
        $("[data-receive-kit]").click(function() {
          $("#kitToReceive").val($(this).data("receive-kit"));
          var fromId = $(this).data("kit-from");
          
          $.getJSON( "<c:url value="/organizations?type=LAW_ENFORCEMENT&type=MEDICAL&type=LAB"/>", function(orgs) {
            sakTrack.initOrganizationSelect(orgs, "#receiveFromSelect");
            $("#receiveFromSelect").val(fromId);
          });
        });
      });
    </script>
  </jsp:attribute>
</t:page>