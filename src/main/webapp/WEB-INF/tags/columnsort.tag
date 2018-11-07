<%@tag trimDirectiveWhitespaces="true" %>
<%@ taglib prefix="t" tagdir="/WEB-INF/tags" %>
<%@taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions" %>

<%@attribute name="sort" required="true" type="org.springframework.data.domain.Sort" %>
<%@attribute name="propertyName" required="true" type="java.lang.String" %>
<%@attribute name="text" required="true" type="java.lang.String" %>
<%@attribute name="sortParameterName" required="false" type="java.lang.String" %>

<c:if test="${not empty sort.getOrderFor(propertyName).direction}">
  <c:if test="${sort.getOrderFor(propertyName).direction == 'ASC'}">
    <c:set var="direction" value=",DESC"/>
    <c:set var="arrow" value="glyphicon glyphicon-chevron-up"/>
  </c:if>
  <c:if test="${sort.getOrderFor(propertyName).direction == 'DESC'}">
    <c:set var="direction" value=",ASC"/>
    <c:set var="arrow" value="glyphicon glyphicon-chevron-down"/>
  </c:if>
</c:if>

<c:if test="${empty sortParameterName}">
  <c:set var="sortParameterName" value="sort"/>
</c:if>

<c:set var="pagingUrl"><t:currenturl excludeParameterName="${sortParameterName}" /></c:set>
<c:url context="/" var="searchUrl" value="${pagingUrl}">
  <c:param name="${sortParameterName}" value="${propertyName}${direction}"/>
</c:url>

<a style="white-space: nowrap" href="${searchUrl}">${text}&nbsp;<span class="${arrow}"></span></a>