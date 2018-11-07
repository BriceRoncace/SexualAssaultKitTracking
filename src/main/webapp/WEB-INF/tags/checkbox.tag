<%@tag trimDirectiveWhitespaces="true" %>
<%@taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>

<%@attribute name="name" required="true" type="java.lang.String" %>
<%@attribute name="id" required="true" type="java.lang.String" %>
<%@attribute name="checked" required="false" type="java.lang.Boolean" %>
<%@attribute name="cssClass" required="false" type="java.lang.String" %>
<%@attribute name="disabled" required="false" type="java.lang.Boolean" %>
<%@attribute name="dataAttributes" required="false" type="java.lang.String" %>

<input type="checkBox" id="<c:out value="${id}"/>" onchange="$('#<c:out value="${id}"/>-hidden').val(this.checked ? 'true' : 'false');" <c:out value="${dataAttributes}"/> class="<c:out value="${cssClass}"/>" ${checked == true ? ' checked ' : ''} ${disabled == true ? ' disabled ' : ''} value="true"/><input type="hidden" id="<c:out value="${id}"/>-hidden" name="<c:out value="${name}"/>" value="${checked == true ? 'true' : 'false'}">