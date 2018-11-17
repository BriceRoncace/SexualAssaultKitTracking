<%@page contentType="text/html" pageEncoding="UTF-8"%>
<%@ taglib prefix="t" tagdir="/WEB-INF/tags" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>

<t:page>
  <jsp:attribute name="body">
    <c:url var="newUser" value="${adminUser.id == null}"/>
    
    <%@include file="includes/admin-nav.jspf" %>
    <%@include file="../../includes/messages.jspf" %>
        
    <ol class="breadcrumb">
      <li><a href="<c:url value="/${user.type.label}/dashboard"/>">Dashboard</a></li>
      <li><a href="<c:url value="/adminUser"/>">Manage Admins</a></li>
      <li class="active">${newUser ? 'New' : 'Edit'} Administrator</li>
    </ol>
    
    <div class="col-sm-offset-3 col-sm-6">
      <div class="panel panel-primary">
        <div class="panel-heading">
          ${newUser ? 'New' : 'Edit'}&nbsp;Administrator
        </div>

        <form id="userForm" action="<c:url value="/adminUser/save"/>" method="POST">
          <input type="hidden" name="${_csrf.parameterName}" value="${_csrf.token}"/>
          <input type="hidden" name="userId" value="${adminUser.id}"/>

          <div class="panel-body">
            <div class="row">
              <div class="col-xs-12">
                <c:choose>
                  <c:when test="${newUser}">
                    <input type="hidden" name="adminUser.enabled" value="true"/>
                  </c:when>
                  <c:otherwise>
                    <div class="checkbox-inline">
                      <label><t:checkbox id="enabled-checkbox" name="adminUser.enabled" checked="${adminUser.id == null || adminUser.enabled}" /> Enabled</label>
                    </div>
                  </c:otherwise>
                </c:choose>
              </div>
            </div>
              
            <div id="setupUser" class="row">
              <div class="col-sm-6">
                <div class="form-group">
                  <label class="control-label required">Username</label>
                  <input id="newUsername" data-required type="text" class="form-control" name="adminUser.username" data-focus-if-empty value="${adminUser.username}"/>
                </div>
                <div class="form-group">
                  <label class="control-label required">Display Name</label>
                  <input type="text" data-required class="form-control" name="adminUser.displayName" value="${adminUser.displayName}"/>
                </div>
                <div class="form-group">
                  <label class="control-label required">Phone</label>
                  <input type="text" data-required class="form-control" name="adminUser.phone" value="${adminUser.phone}"/>
                </div>
              </div>
              <div class="col-sm-6">
                <c:if test="${user.id != null && adminUser.id == user.id}">
                  <div id="currentPassword" class="form-group">
                    <label class="control-label">Current Password</label>
                    <input type="password" class="form-control" name="passwordPair.oldPassword"/>
                  </div>
                </c:if>
                <div class="form-group">
                  <label class="control-label ${newUser ? 'required' : ''}">New Password</label>
                  <input type="password" ${newUser ? 'data-required' : ''} class="form-control" name="passwordPair.passwordOne"/>
                </div>
                <div class="form-group">
                  <label class="control-label ${newUser ? 'required' : ''}">Confirm Password</label>
                  <input type="password" ${newUser ? 'data-required' : ''} class="form-control" name="passwordPair.passwordTwo"/>
                </div>
              </div>
              <div class="col-xs-12">
                <div class="form-group">
                  <label class="control-label required">Email</label>
                  <input type="text" data-required class="form-control" name="adminUser.email" value="${adminUser.email}"/>
                </div>
              </div>
            </div>

          </div>
          <div class="panel-footer text-right">
            <a href="<c:url value="/adminUser"/>" class="btn btn-default">Cancel</a>
            <button type="submit" class="btn btn-primary">Save</button>
          </div>
        </form>

      </div>
    </div>
  </jsp:attribute>
  <jsp:attribute name="scripts">
  </jsp:attribute>  
</t:page>