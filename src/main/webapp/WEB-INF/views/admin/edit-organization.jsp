<%@page contentType="text/html" pageEncoding="UTF-8"%>
<%@ taglib prefix="t" tagdir="/WEB-INF/tags" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>

<t:page>
  <jsp:attribute name="body">
    <%@include file="includes/admin-nav.jspf" %>
    <%@include file="../../includes/messages.jspf" %>
    <t:breadcrumb crumbOrganizationDashboard="${organization}" crumbActive="Manage Organization"/>
    <h2>${organization.id == null ? 'New' : 'Manage'} Organization</h2>
    <%@include file="../../includes/manage-organization.jspf" %>

  </jsp:attribute>
  <jsp:attribute name="scripts">
    <script src="<c:url value="/assets/js/organization-form.js"/>"></script>
  </jsp:attribute>
</t:page>