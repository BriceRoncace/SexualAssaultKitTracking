<%@page contentType="text/html" pageEncoding="UTF-8"%>
<%@ taglib prefix="fmt" uri="http://java.sun.com/jsp/jstl/fmt" %>
<%@taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@taglib prefix="t" tagdir="/WEB-INF/tags" %>

<t:page>
  <jsp:attribute name="body">
    <%@include file="public/includes/public-nav.jspf" %>
    <%@include file="../includes/messages.jspf" %>
    <h2 class="invalid">An error has occurred.</h2>
    <p>We apologize!&nbsp;&nbsp;Your request encountered an unexpected error.</p> 
  </jsp:attribute>
</t:page>