<%@page contentType="text/html" pageEncoding="UTF-8"%>
<%@ taglib prefix="t" tagdir="/WEB-INF/tags" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>

<t:page>
  <jsp:attribute name="body">
    <%@include file="../../includes/org-nav.jspf" %>
    <%@include file="../../includes/messages.jspf" %>
    <t:breadcrumb crumbKit="${kit}" crumbActive="Timeline"/>
    <h2 class="bottom0">Sexual Assault Kit <c:if test="${kit != null}">#${kit.serialNumber}</c:if></h2>
    <h3 class="topNeg10 line-under"><small>Medical View</small></h3>
    <%@include file="../../includes/timeline.jspf" %>
  </jsp:attribute>
</t:page>
