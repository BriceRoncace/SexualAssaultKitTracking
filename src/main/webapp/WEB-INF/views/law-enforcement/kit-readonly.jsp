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
      <span class="pull-right">
        <a class="btn btn-xs btn-default" href="<c:url value="/timeline"/>?serialNumber=${kit.serialNumber}"><i class="fa fa-clock-o font14" aria-hidden="true"></i> Kit Timeline</a>   
      </span>
    </h2>
    
    <div id="readonly-container">
      <%@include file="../../includes/law-enforcement-details.jspf" %>
      <c:if test="${kit.legalDetails.prosecutorAgrees != null}"><%@include file="../../includes/legal-details-readonly.jspf" %></c:if>
    </div>
    
    <div class="row">
      <div class="col-sm-8">
        <b>Status:</b> ${kit.status.label}
      </div>
    </div>
      
  </jsp:attribute>
  <jsp:attribute name="scripts">
    <script type="text/javascript">
    $(function() {
      $("#readonly-container").find("input, select, checkbox").attr("disabled", true);
    });
    </script>
  </jsp:attribute>
</t:page>
