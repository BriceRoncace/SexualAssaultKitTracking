<%@page contentType="text/html" pageEncoding="UTF-8"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="t" tagdir="/WEB-INF/tags" %>
<t:page>
  <jsp:attribute name="body">
    <%@include file="includes/public-nav.jspf" %>
    <%@include file="../../includes/messages.jspf" %>
    <%@include file="../../includes/password-requirements.jspf" %>

    <div class="col-sm-offset-4 col-sm-4">
      <div class="panel panel-primary">
        <div class="panel-heading">Reset Password</div>
        <form action="#" method="POST">
          <input type="hidden" name="${_csrf.parameterName}" value="${_csrf.token}"/>
          <div class="panel-body">
            <div class="form-group">
              <label class="control-label required">New Password</label>
              <input type="password" data-required class="form-control" name="passwordOne"/>
            </div>
            <div class="form-group">
              <label class="control-label required">Confirm Password</label>
              <input type="password" data-required class="form-control" name="passwordTwo"/>
            </div>
          </div>
          <div class="panel-footer text-right">
            <button type="submit" class="btn btn-primary">Submit</button>
          </div>
        </form>
      </div>
    </div>

  </jsp:attribute>
</t:page>