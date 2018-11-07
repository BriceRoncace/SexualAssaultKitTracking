<%@tag trimDirectiveWhitespaces="true" %>
<%@taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@attribute name="crumbActive" required="true" type="java.lang.String" %>
<%@attribute name="crumbOrganizationDashboard" required="false" type="gov.idaho.isp.saktrack.domain.organization.Organization" %>
<%@attribute name="crumbOrganizationManagement" required="false" type="gov.idaho.isp.saktrack.domain.organization.Organization" %>
<%@attribute name="crumbKit" required="false" type="gov.idaho.isp.saktrack.domain.SexualAssaultKit" %>

<ol class="breadcrumb">
  <li><a href="<c:url value="/${user.type.label}/dashboard"/>">dashboard</a></li>
  <c:if test="${crumbOrganizationDashboard != null}">
    <li><a href="<c:url value="/${crumbOrganizationDashboard.type.devLabel}/dashboard"/>?orgId=${crumbOrganizationDashboard.id}"><c:out value="${crumbOrganizationDashboard.name}"/> Dashboard</a></li>
  </c:if>
  <c:if test="${crumbOrganizationManagement != null}">
    <li><a href="<c:url value="/organization/"/>${crumbOrganizationManagement.id}">Manage Organization</a></li>
  </c:if>  
  <c:if test="${crumbKit != null}">
    <li><a href="<c:url value="/${user.type.label}/view"/>?id=${crumbKit.id}">Kit #${crumbKit.serialNumber}</a></li>
  </c:if>  
    <li class="active"><c:out value="${crumbActive}"/></li>
</ol>