<%@page contentType="text/html" pageEncoding="UTF-8"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="cjisTags" uri="http://isp.idaho.gov/jsp/cjisTags"%>
<%@ taglib prefix="t" tagdir="/WEB-INF/tags" %>
<t:page>
  <jsp:attribute name="body">
    <%@include file="includes/admin-nav.jspf" %>
    <%@include file="../../includes/messages.jspf" %>
    <t:breadcrumb crumbOrganizationDashboard="${kit.currentAssignment}" crumbKit="${kit}" crumbActive="Timeline"/>
    <h2 class="bottom0">Sexual Assault Kit <c:if test="${kit != null}">#${kit.serialNumber} <%@include file="includes/kit-actions-btn-dropdown.jspf" %></c:if></h2>
    <h3 class="topNeg10 line-under"><small> Forensic Services Administrator View</small></h3>
    <%@include file="../../includes/timeline.jspf" %>
  </jsp:attribute>
</t:page>
