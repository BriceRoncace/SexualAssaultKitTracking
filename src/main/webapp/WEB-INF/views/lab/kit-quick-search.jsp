<%@page contentType="text/html" pageEncoding="UTF-8"%>
<%@ taglib prefix="t" tagdir="/WEB-INF/tags" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>

<t:page>
  <jsp:attribute name="body">
    <%@include file="../../includes/org-nav.jspf" %>
    <%@include file="../../includes/messages.jspf" %>
    <t:breadcrumb crumbActive="Quick Search"/>
    <h2 class="line-under">Multiple Sexual Assault Kits were found.</h2>
    <%@include file="../../includes/search-results.jspf" %>
  </jsp:attribute>
</t:page>