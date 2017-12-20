<%@page contentType="text/html" pageEncoding="UTF-8"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="t" tagdir="/WEB-INF/tags" %>
<t:page>
  <jsp:attribute name="body">
    <%@include file="../../includes/org-nav.jspf" %>
    <%@include file="../../includes/messages.jspf" %>
    <c:if test="${user.admin}">
      <t:breadcrumb crumbOrganizationDashboard="${organization}" crumbActive="Manage Organization"/>
    </c:if>
    <c:if test="${!user.admin}">
      <t:breadcrumb crumbActive="Manage Organization"/>
    </c:if>
    
    <h2>Manage Organization</h2>
    <%@include file="../../includes/manage-organization.jspf" %>
  </jsp:attribute>
  <jsp:attribute name="scripts">
    <script src="<c:url value="/assets/js/organization-form.js"/>"></script>
  </jsp:attribute>
</t:page>