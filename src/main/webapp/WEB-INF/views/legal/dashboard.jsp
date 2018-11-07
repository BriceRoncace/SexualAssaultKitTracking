<%@page contentType="text/html" pageEncoding="UTF-8"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="t" tagdir="/WEB-INF/tags" %>
<t:page>
  <jsp:attribute name="head">
    <link href="<c:url value="/assets/css/pagination.css"/>" rel="stylesheet" type="text/css" />
  </jsp:attribute>
  <jsp:attribute name="body">
    <%@include file="../../includes/org-nav.jspf" %>
    <%@include file="../../includes/messages.jspf" %>
    <c:if test="${user.admin}"><t:breadcrumb crumbActive="${organization.name} Dashboard"/></c:if>
    <h2><c:out value="${organization.name}"/> Dashboard</h2>
    
    <div class="row top5">
      <div class="col-sm-12">
        <c:choose>
          <c:when test="${prosecutorKits == null || prosecutorKits.totalElements == 0}">
            <div class="col-xs-12 font12">
              There are currently no sexual assault kits in need of processing.
            </div>
          </c:when>
          <c:otherwise>
            
            <table class="table table-striped">
              <thead>
              <tr>
                <th style="width: 13%;"><t:columnsort sort="${prosecutorKits.sort}" sortParameterName="sort" propertyName="serialNumber" text="Serial #"/></th>
                <th><t:columnsort sort="${prosecutorKits.sort}" sortParameterName="sort" propertyName="crimeDate" text="Crime Date"/></th>
                <th style="width: 17%;"><t:columnsort sort="${prosecutorKits.sort}" sortParameterName="sort" propertyName="currentAssignment" text="Agency"/></th>
                <th><t:columnsort sort="${prosecutorKits.sort}" sortParameterName="sort" propertyName="leCaseNumber" text="LE Case #"/></th>
                <th><t:columnsort sort="${prosecutorKits.sort}" sortParameterName="sort" propertyName="nonSubReason" text="Non-Submission Reason"/></th>
                <th class="noSort"/>
              </tr>
              </thead>
              <tbody>
                <c:forEach var="kit" items="${prosecutorKits.content}">
                  <tr>
                    <td><a href="<c:url value="/legal/view"/>?id=${kit.id}">${kit.serialNumber}</a></td>
                    <td>
                      ${dateFormatter.format(kit.leDetails.crimeDate)}
                    </td>
                    <td><c:out value="${kit.currentAssignment.name}"/></td>
                    <td>${kit.leDetails.caseNumber}</td>
                    <td>${kit.legalDetails.nonSubmissionReason.label}</td>
                    <td style="width: 5%">
                      <div class="dropdown">
                        <a href="javascript://nop/" data-toggle="dropdown"><span class="font18 glyphicon glyphicon-option-horizontal"></span></a>
                        <ul class="dropdown-menu dropdown-menu-right">
                          <li><a href="<c:url value="/legal/view"/>?id=${kit.id}"><span class="glyphicon glyphicon-pencil"></span> Edit</a></li>
                          <li><a href="<c:url value="/timeline"/>?serialNumber=${kit.serialNumber}"><i class="fa fa-clock-o font14" aria-hidden="true"></i> Kit Timeline</a></li>
                        </ul>
                      </div>
                    </td>
                  </tr>
                </c:forEach>
              </tbody>
            </table>
            
            <c:if test="${prosecutorKits.size < prosecutorKits.totalElements}">
              <t:pager page="${prosecutorKits}" showInfo="true"/>
           </c:if>
           
          </c:otherwise>
        </c:choose>
      </div>
    </div>    
    
  </jsp:attribute>
</t:page>