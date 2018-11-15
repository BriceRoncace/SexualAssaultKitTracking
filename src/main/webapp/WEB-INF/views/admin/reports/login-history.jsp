<%@page contentType="text/html" pageEncoding="UTF-8"%>
<%@ taglib prefix="t" tagdir="/WEB-INF/tags" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>

<t:page>
  <jsp:attribute name="body">
    <%@include file="../includes/admin-nav.jspf" %>
    <%@include file="../../../includes/messages.jspf" %>
    
    <div class="panel panel-primary">
      <div class="panel-heading"><h2 class="panel-title">User Login History</h2> </div>
      <div class="panel-body">
        <form action="<c:url value="/report/loginHistory"/>" method="GET">
          <div class="row hidden-print">
            
            <div class="col-xs-6">
              <div class="form-group">
                <label class="control-label">Organization</label>
                <t:select name="organizationId" from="${organizations}" emptyOption="" cssClass="form-control" optionKey="id" optionValue="name" value="${spec.organizationId}"/>
              </div>
            </div>
            <div class="col-xs-6">
              <div class="form-group">
                <label class="control-label">Login Date</label>
                <t:criteria-date name="loginDate" value="${spec.loginDate}"/>
              </div>
            </div>
            
            <div class="col-xs-3">
              <div class="form-group">
                <label class="control-label">Display Name</label>
                <input type="text" name="displayName" class="form-control" value="<c:out value="${spec.displayName}"/>"/>
              </div>
            </div>  
            <div class="col-xs-3">
              <div class="form-group">
                <label class="control-label">Username</label>
                <input type="text" name="username" class="form-control" value="<c:out value="${spec.username}"/>"/>
              </div>
            </div>  
            <div class="col-xs-3">
              <div class="form-group">
                <label class="control-label">Email</label>
                <input type="text" name="email" class="form-control" value="<c:out value="${spec.email}"/>"/>
              </div>
            </div>    
              
            <div class="col-xs-3 top25 text-center">
              <button data-action="<c:url value="/report/loginHistory"/>" type="submit" class="btn btn-primary">Search</button>&nbsp;<button data-action="<c:url value="/report/loginHistory/download"/>" type="submit" class="btn btn-default"><i class="fa fa-arrow-circle-o-down font18" title="download"></i> Export</button>
            </div>
          </div>
        </form>
      </div>
         
      <table class="table table-hover">
        <thead>
          <tr>
            <th><t:columnsort sort="${page.sort}" propertyName="userType" text="User Type"/></th>
            <th><t:columnsort sort="${page.sort}" propertyName="displayName" text="Display Name"/></th>
            <th><t:columnsort sort="${page.sort}" propertyName="username" text="Username"/></th>
            <th><t:columnsort sort="${page.sort}" propertyName="email" text="Email"/></th>
            <th><t:columnsort sort="${page.sort}" propertyName="organization" text="Organiazation"/></th>
            <th><t:columnsort sort="${page.sort}" propertyName="loginTime" text="Login Time"/></th>
          </tr>
        </thead>
        <tbody>
          <c:forEach var="userLogin" items="${page.content}">
            <tr>
              <td><c:out value="${userLogin.userType.label}"/></td>
              <td><c:out value="${userLogin.displayName}"/></td>
              <td><c:out value="${userLogin.username}"/></td>
              <td><a href="mailto:<c:out value="${userLogin.email}"/>"><c:out value="${userLogin.email}"/></a></td>
              <td>
                <c:choose>
                  <c:when test="${userLogin.userType == 'ADMIN'}">
                    <a href="<c:url value="/adminUser"/>"><em>none</em></a>
                  </c:when>
                  <c:otherwise>
                    <a href="<c:url value="/organization/${userLogin.organizationId}"/>"><c:out value="${userLogin.organization}"/></a>
                  </c:otherwise>
                </c:choose>
              </td>
              <td>${dateTimeFormatter.format(userLogin.loginTime)}</td>
            </tr>
          </c:forEach>
        </tbody>
        <tfoot></tfoot>
      </table>      
    </div>
    
    <div class="top5">
      <t:pager page="${page}" showInfo="true"/>
    </div>
            
  </jsp:attribute>
  <jsp:attribute name="scripts">
  </jsp:attribute>
</t:page>
