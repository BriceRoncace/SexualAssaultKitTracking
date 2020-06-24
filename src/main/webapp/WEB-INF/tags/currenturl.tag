<%@taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>

<%@attribute name="excludeParameterNames" required="false" type="java.lang.String" %>

<c:set var="paramValueFound" value="false"/>
<c:url value="${requestScope['javax.servlet.forward.servlet_path']}">
  <c:forEach var="entry" items="${paramValues}">
    <c:set var="exclude" value="false"/>
    <c:forTokens items="${excludeParameterNames}" delims=", " var="excludedParameterName">
      <c:if test="${entry.key == excludedParameterName}">
        <c:set var="exclude" value="true"/>
      </c:if>
    </c:forTokens>
    
    <c:if test="${!exclude}">
      <c:forEach var="val" items="${entry.value}">
        <c:param name="${entry.key}" value="${val}" />
        <c:set var="paramValueFound" value="true"/>
      </c:forEach>
    </c:if>
  </c:forEach>
  
  <c:if test="${!paramValueFound}">
    <c:param name="t" />
  </c:if>
</c:url>