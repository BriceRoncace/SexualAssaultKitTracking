<%@page contentType="text/html" pageEncoding="UTF-8"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="cjisTags" uri="http://isp.idaho.gov/jsp/cjisTags"%>
<%@ taglib prefix="t" tagdir="/WEB-INF/tags" %>
<t:page>
  <jsp:attribute name="body">
    <%@include file="includes/public-nav.jspf" %>
    <%@include file="../../includes/messages.jspf" %>
    
    <c:choose>
      <c:when test="${kit == null || empty kit.chainOfCustody}">
        No records found.
      </c:when>
      <c:otherwise>
        <h2 class="line-under">Sexual Assault Kit #${kit.serialNumber}</h2>
        
        <%@include file="../../includes/chain-graphic.jspf" %>
        <hr/>
        <%@include file="../../includes/extra-timeline-info.jspf" %>
        <ul class="list-group">
          <c:forEach var="coc" items="${kit.chainOfCustody}">
            <li class="list-group-item">
              <h4 class="list-group-item-heading text-capitalize">${coc.eventType.pastTenseLabel}&nbsp;<cjisTags:fmtTemporal value="${coc.eventDate}" pattern="${dateFormat}"/></h4>
              <c:out value="${coc.getDescription(false)}"/>
            </li>
          </c:forEach>
        </ul>
      </c:otherwise>
    </c:choose>
  </jsp:attribute>
</t:page>
