<%@taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>

<%@attribute name="excludeParameterName" required="false" type="java.lang.String" %>

<c:url value="${requestScope['javax.servlet.forward.servlet_path']}">
  <c:if test="${empty paramValues}">
    <c:param name="t" />
  </c:if>
  
  <c:forEach var="entry" items="${paramValues}">
    <c:if test="${entry.key != excludeParameterName}">
      <c:param name="${entry.key}" value="${entry.value[0]}" />
    </c:if>
  </c:forEach>
</c:url>