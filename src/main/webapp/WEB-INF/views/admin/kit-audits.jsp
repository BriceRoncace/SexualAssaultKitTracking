<%@page contentType="text/html" pageEncoding="UTF-8"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="cjisTags" uri="http://isp.idaho.gov/jsp/cjisTags"%>
<%@ taglib prefix="t" tagdir="/WEB-INF/tags" %>
<t:page>
  <jsp:attribute name="body">
    <%@include file="includes/admin-nav.jspf" %>
    <%@include file="../../includes/messages.jspf" %>
    <t:breadcrumb crumbOrganizationDashboard="${kit.currentAssignment}" crumbKit="${kit}" crumbActive="Admin Change Log"/>
    <h2 class="bottom0">Sexual Assault Kit #${kit.serialNumber} <%@include file="includes/kit-actions-btn-dropdown.jspf" %></h2>
    <h3 class="topNeg10 line-under"><small> Admin Change Log</small></h3>
    
    <ul class="list-group">
      <c:forEach var="audit" items="${audits}">
        <li class="list-group-item font14">
          <h4 class="list-group-item-heading">
            ${audit.displayName} - <cjisTags:fmtTemporal value="${audit.modified}" pattern="${dateTimeFormat}"/> 
          </h4>
            
          <ul>
            <c:forEach var="change" items="${audit.changes}">
              <li><c:out value="${change}"/></li>
            </c:forEach>
          </ul>
          <blockquote class="blockquote-reverse no-margin font12"><c:out value="${audit.notes}"/></blockquote>
        </li>
      </c:forEach>
    </ul>
  </jsp:attribute>
</t:page>