<%@page contentType="text/html" pageEncoding="UTF-8"%>
<%@ taglib prefix="t" tagdir="/WEB-INF/tags" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>

<t:page>
  <jsp:attribute name="css">
    <link href="<c:url value="/assets/css/typeahead.js-bootstrap.css"/>" rel="stylesheet" type="text/css" />
  </jsp:attribute>
  <jsp:attribute name="body">
    <%@include file="../../includes/org-nav.jspf" %>
    <%@include file="../../includes/messages.jspf" %>
    <c:if test="${user.admin}">
      <t:breadcrumb crumbOrganizationDashboard="${organization}" crumbOrganizationManagement="${organization}" crumbActive="Manage User"/>
    </c:if>
    <c:if test="${!user.admin}">
      <t:breadcrumb crumbOrganizationManagement="${user.organizationAdmin ? organization : null}" crumbActive="Manage User"/>
    </c:if>
    
    <%@include file="../../includes/password-requirements.jspf" %>
    
    <c:choose>
      <c:when test="${user.admin || user.organizationAdmin}">
        <c:url var="saveUser" value="/organization/${organization.id}/user/save"/>
        <c:url var="cancelLink" value="/organization/${organization.id}"/>
      </c:when>
      <c:otherwise>
        <c:url var="saveUser" value="/manageAccount/db"/>
        <c:url var="cancelLink" value="/${user.type.label}/dashboard"/>
      </c:otherwise>
    </c:choose>
    
    <c:url var="newUser" value="${orgUser.id == null}"/>
    
    <div class="col-sm-offset-3 col-sm-6">
      <div class="panel panel-primary">
        <div class="panel-heading">
          ${newUser ? 'New' : 'Edit'}&nbsp;User
          <span class="pull-right">
            <button type="button" class="btn btn-xs btn-info topNeg3" data-toggle="modal" data-target="#userPreferences">
              <span class="glyphicon glyphicon-cog"></span> Preferences
            </button>
          </span>
        </div>
        <form id="userForm" action="${saveUser}" method="POST">
          <input type="hidden" name="${_csrf.parameterName}" value="${_csrf.token}"/>
          <input type="hidden" name="orgId" value="${organization.id}"/>
          <input type="hidden" name="userId" value="${orgUser.id}"/>
          
          <div class="panel-body">
            <c:if test="${user.isOrganizationAdmin()}">
              <div class="row">
                <div class="col-xs-12">
                  <c:choose>
                    <c:when test="${newUser}">
                      <input type="hidden" name="orgUser.enabled" value="true"/>
                    </c:when>
                    <c:otherwise>
                      <div class="checkbox-inline">
                        <label><t:checkbox id="enabled-checkbox" name="orgUser.enabled" checked="${orgUser.id == null || orgUser.enabled}" /> Enabled</label>
                      </div>
                    </c:otherwise>
                  </c:choose>
                  <div class="checkbox-inline">
                    <label><t:checkbox id="orgAdmin-checkbox" name="orgUser.organizationAdmin" checked="${orgUser.isOrganizationAdmin()}" /> Organization Administrator</label>
                  </div>
                    <div class="checkbox-inline">
                      <label><t:checkbox id="orgContact-checkbox" name="orgUser.organizationContact" checked="${orgUser.isOrganizationContact()}" /> Organization Contact</label>
                    </div>
                </div>
              </div>
            </c:if>
            
            <div id="setupUser" class="row">
              <div class="col-sm-6">
                <div class="form-group">
                  <label class="control-label required">Username</label>
                  <input id="newUsername" data-required type="text" class="form-control" name="orgUser.username" data-focus-if-empty value="${orgUser.username}"/>
                </div>
                <div class="form-group">
                  <label class="control-label required">Display Name</label>
                  <input type="text" data-required class="form-control" name="orgUser.displayName" value="${orgUser.displayName}"/>
                </div>
                <div class="form-group">
                  <label class="control-label required">Phone</label>
                  <input type="text" data-required class="form-control" name="orgUser.phone" value="${orgUser.phone}"/>
                </div>
              </div>
              <div class="col-sm-6">
                <c:if test="${orgUser.id == user.id && !user.isOrganizationAdmin()}">
                  <div class="form-group">
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
                  <input type="text" data-required class="form-control" name="orgUser.email" value="${orgUser.email}"/>
                </div>
              </div>
            </div>
            
          </div>
          <div class="panel-footer text-right">
            <a href="${cancelLink}" class="btn btn-default">Cancel</a>
            <button type="submit" class="btn btn-primary">${orgUser.verifiedDate != null || orgUser.id == null ? 'Save' : 'Verify'}</button>
          </div>
          <%@include file="../../includes/user-preferences-dialog.jspf" %>
        </form>
      </div>
    </div>

  </jsp:attribute>
  <jsp:attribute name="scripts">
  </jsp:attribute>
</t:page>