<%@tag trimDirectiveWhitespaces="true" %>
<%@taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@tag description="Outputs select html element" pageEncoding="UTF-8"%>

<%@attribute name="name" required="true" type="java.lang.String" %>
<%@attribute name="from" required="true" type="java.lang.Object" %>
<%@attribute name="cssClass" required="false" type="java.lang.String" %>
<%@attribute name="dataAttributes" required="false" type="java.lang.String" %>
<%@attribute name="emptyOption" required="false" type="java.lang.String" %>
<%@attribute name="multiple" required="false" type="java.lang.Boolean" %>
<%@attribute name="size" required="false" type="java.lang.Integer" %>
<%@attribute name="disabled" required="false" type="java.lang.Boolean" %>
<%@attribute name="id" required="false" type="java.lang.String" %>
<%@attribute name="value" required="false" type="java.lang.Object" %>
<%@attribute name="optionKey" required="false" type="java.lang.String" %>
<%@attribute name="optionValue" required="false" type="java.lang.String" %>
<%@attribute name="optionValueEscapeXml" required="false" type="java.lang.String" %>

<select <c:if test="${multiple != null}">multiple</c:if> 
        <c:if test="${size != null}">size="<c:out value="${size}"/>"</c:if> 
        <c:if test="${disabled == true}">disabled</c:if> 
        <c:if test="${id != null}">id="<c:out value="${id}"/>"</c:if> 
        name="<c:out value="${name}"/>" 
        <c:if test="${cssClass != null}">class="<c:out value="${cssClass}"/>"</c:if>
        <c:if test="${dataAttributes != null}"><c:out value="${dataAttributes}"/></c:if>>
  <c:if test="${emptyOption != null}"><option value=""><c:out value="${emptyOption}"/></option></c:if>
  <c:forEach var="item" items="${from}">
    <c:set var="itemValue" value="${optionKey != null ? item[optionKey] : item}"/>
    <%
      if (shouldSelect(value, jspContext.getAttribute("itemValue"))) {
        jspContext.setAttribute("selected", true);
      }
      else {
        jspContext.setAttribute("selected", false);
      }
    %>
    <option <c:if test="${selected}">selected</c:if> value="<c:out value="${optionKey != null ? item[optionKey] : item}"/>"><c:out value="${optionValue != null ? item[optionValue] : item}" escapeXml="${optionValueEscapeXml != null ? optionValueEscapeXml : true}"/></option>
  </c:forEach>
</select>
    
<%!
private boolean shouldSelect(Object selectedValues, Object value) {
  if (selectedValues != null) {
    if (selectedValues instanceof Iterable) {
      for (Object v : (Iterable) selectedValues) {
        if (v != null && v.equals(value)) {
          return true;
        }
      }
    }
    if (selectedValues instanceof String) {
      return selectedValues.equals(value.toString());
    }
    else {
      return selectedValues.equals(value);
    }
  }
  return false;
}
%>