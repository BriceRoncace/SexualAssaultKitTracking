<%@page contentType="text/html" pageEncoding="UTF-8"%>
<%@ taglib prefix="t" tagdir="/WEB-INF/tags" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>

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
      <%@include file="../../includes/expiration-date.jspf" %>
      <%@include file="../../includes/medical-details.jspf" %>
    </div>
      
  </jsp:attribute>
  <jsp:attribute name="scripts">
    <script type="text/javascript">
    $(function() {
      $("#readonly-container").find("input, select").attr("disabled", true);
    });
    </script>
  </jsp:attribute>
</t:page>