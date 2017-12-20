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
    
   <form action="<c:url value='/lab/saveDetails'/>" method="POST">
      <input type="hidden" name="${_csrf.parameterName}" value="${_csrf.token}"/>
      <input type="hidden" name="id" value="${kit.id}"/>
      
      <div id="readonly-container">
        <%@include file="../../includes/expiration-date.jspf" %>
        <%@include file="../../includes/lab-details.jspf" %>
      </div>
      
      <div class="row">
        <div class="col-sm-12">
          <div class="top10 pull-right">
            <button type="submit" id="submit-btn" class="btn btn-primary hidden">Save</button>
          </div>
        </div>
      </div>
   </form>

  </jsp:attribute>
  <jsp:attribute name="scripts">
    <script type="text/javascript">
    $(function() {
      $("#readonly-container").find("input").not("[data-always-editable]").attr("disabled", true);
      if ($("#readonly-container").find("input:enabled").not(":hidden").length > 0) {
        $("#submit-btn").removeClass("hidden");
      }
    });
    </script>
  </jsp:attribute>
</t:page>