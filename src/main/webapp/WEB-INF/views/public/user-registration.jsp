<%@page contentType="text/html" pageEncoding="UTF-8"%>
<%@ taglib prefix="t" tagdir="/WEB-INF/tags" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>

<t:page>
  <jsp:attribute name="body">
    <%@include file="includes/public-nav.jspf" %>
    <%@include file="../../includes/messages.jspf" %>
    <%@include file="../../includes/password-requirements.jspf" %>
    
    <div class="col-sm-offset-3 col-sm-6">
      <div class="panel panel-primary">
        <div class="panel-heading">Register</div>
        <form action="<c:url value="/register"/>" method="POST">
          <input type="hidden" name="${_csrf.parameterName}" value="${_csrf.token}"/>
          <div class="panel-body">
            <div class="col-sm-6">
              <div class="form-group">
                <label class="control-label required">Username</label>
                <input type="text" class="form-control" data-required name="orgUser.username" data-focus value="${orgUser.username}"/>
              </div>
              <div class="form-group">
                <label class="control-label required">Display Name</label>
                <input type="text" class="form-control" data-required name="orgUser.displayName" value="${orgUser.displayName}"/>
              </div>
              <div class="form-group">
                <label class="control-label required">Phone</label>
                <input type="text" class="form-control" name="orgUser.phone" value="${orgUser.phone}"/>
              </div>
            </div>
            <div class="col-sm-6">
              <div class="form-group">
                <label class="control-label ${orgUser.id == null ? 'required' : ''}">New Password</label>
                <input type="password" class="form-control" data-required name="passwordPair.passwordOne"/>
              </div>
              <div class="form-group">
                <label class="control-label ${orgUser.id == null ? 'required' : ''}">Confirm Password</label>
                <input type="password" class="form-control" data-required name="passwordPair.passwordTwo"/>
              </div>
            </div>
            <div class="col-xs-12">
              <div class="form-group">
                <label class="control-label required">Email</label>
                <input type="text" class="form-control" name="orgUser.email" data-required value="${orgUser.email}"/>
              </div>
            </div>
            <div class="col-sm-6">
              <div class="form-group">
                <label class="control-label required">Organization</label>
                <t:select from="${organizations}" name="orgId" optionKey="id" optionValue="name" value="${orgUser.organization.id}" emptyOption="" dataAttributes="data-required" cssClass="form-control"/>
              </div>
            </div>
            <div class="col-sm-6">
              <div class="form-group">
                <label class="control-label required">Passkey</label>
                <input type="text" class="form-control" name="orgUser.passkey" data-required value="${orgUser.passkey}"/>
              </div>
            </div>
          </div>
          <div class="panel-footer text-right">
            <a href="<c:url value="/login"/>" class="btn btn-default">Cancel</a>
            <button type="submit" class="btn btn-primary">Save</button>
          </div>
        </form>
      </div>
    </div>

  </jsp:attribute>
</t:page>