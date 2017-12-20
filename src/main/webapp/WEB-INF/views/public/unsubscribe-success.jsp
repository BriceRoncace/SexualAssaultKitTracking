<%@page contentType="text/html" pageEncoding="UTF-8"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="t" tagdir="/WEB-INF/tags" %>
<t:page>
  <jsp:attribute name="body">
    <%@include file="includes/public-nav.jspf" %>
    <%@include file="../../includes/messages.jspf" %>
    
    <div class="col-sm-offset-3 col-sm-6">
      <h3>You have unsubscribed successfully.</h3>
      <p>If you want to subscribe to this notification again, please login and click on manage. Under preferences you can check the box to re-subscribe.</p>
      <div class="text-center"><a href="<c:url value="/"/>" class="btn btn-primary">Home</a></div>
    </div>

  </jsp:attribute>
</t:page>