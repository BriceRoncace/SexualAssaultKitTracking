<%@page contentType="text/html" pageEncoding="UTF-8"%>
<%@ taglib prefix="t" tagdir="/WEB-INF/tags" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="fmt" uri="http://java.sun.com/jsp/jstl/fmt" %>

<t:page>
  <jsp:attribute name="body">
    <%@include file="includes/public-nav.jspf" %>
    <%@include file="../../includes/messages.jspf" %>
    <c:if test="${param.error != null}">
      <div class="alert alert-danger">
        <c:choose>
          <c:when test="${SPRING_SECURITY_LAST_EXCEPTION.message == 'User is disabled'}">
            User or Organization has been disabled
          </c:when>
          <c:when test="${SPRING_SECURITY_LAST_EXCEPTION.message == 'User account is locked'}">
            User account has not been verified.
          </c:when>
          <c:otherwise>
            Invalid username or password
          </c:otherwise>
        </c:choose>

      </div>
    </c:if>
    <c:if test="${param.logout != null}">
      <div class="alert alert-success">You have been logged out.</div>
    </c:if>

    <form action="<c:url value="/login"/>" method="post" class="form-horizontal">
      <input type="hidden" name="${_csrf.parameterName}" value="${_csrf.token}"/>
      <div class="form-group form-group-sm">
        <label class="col-sm-2 control-label">Username:</label>
        <div class="col-sm-4"><input type="text" id="username" name="username" class="form-control" data-focus/> </div>
      </div>
      <div class="form-group form-group-sm">
        <label class="col-sm-2 control-label">Password:</label>
        <div class="col-sm-4">
          <input type="password" id="password" name="password" class="form-control"/>
          <small><a href="#resetDialog" data-toggle="modal">I forgot my password</a></small>
        </div>
      </div>
      <div class="form-group form-group-sm">
        <div class="col-sm-4 col-sm-offset-2"><button type="submit" class="btn btn-primary">Log in</button></div>
      </div>
      <div class="form-group form-group-sm">
        <div class="col-sm-4 col-sm-offset-2">
          <hr/>
          <small>If you do not already have an account but have the passkey for your organization, you may <a href="<c:url value="/register"/>">register here</a>.</small>
        </div>
      </div>
    </form>  

    <c:if test="${activeProfile == 'dev'}">
      <div class="alert alert-info alert-dismissible" role="alert">
        <button type="button" class="close" data-dismiss="alert" aria-label="Close"><span aria-hidden="true">&times;</span></button>
        <div class="row">
          <div class="col-sm-offset-2 col-sm-6 col-xs-12 bottom10">
            <h4>Development Mode</h4> 
            <a href="#" class="alert-link" onclick="$('#seedDemoData').submit();">Click here to seed the database with demo data.</a>
          </div>
        </div>

        <div class="row">
          <div class="col-sm-offset-2 col-sm-6 col-xs-12">
            <table class="table">
              <thead>
                <tr>
                  <th>Role</th>
                  <th>Username</th>
                  <th>Password</th>
                </tr>
              </thead>
              <tbody>
                <tr>
                  <td>Administrator (in memory)</td>
                  <td>admin</td>
                  <td>admin</td>
                </tr>
                <tr>
                  <td>Administrator</td>
                  <td>administrator</td>
                  <td>P@ssword1</td>
                </tr>
                <tr>
                  <td>Lab User</td>
                  <td>lab</td>
                  <td>P@ssword1</td>
                </tr>
                <tr>
                  <td>Medical User</td>
                  <td>medical</td>
                  <td>P@ssword1</td>
                </tr>
                <tr>
                  <td>Law Enforcement User</td>
                  <td>law</td>
                  <td>P@ssword1</td>
                </tr>
              </tbody>
              <tfoot>
              </tfoot>
            </table>
          </div>          
        </div>
        
        <form id="seedDemoData" action="<c:url value="/seedDemoData"/>" method="POST">
          <input type="hidden" name="${_csrf.parameterName}" value="${_csrf.token}"/>
        </form>
      </div>
    </c:if>

    <div id="resetDialog" class="modal fade" tabindex="-1" role="dialog">
      <div class="modal-dialog modal-sm" role="document">
        <div class="modal-content">
          <div class="modal-header">
            <button type="button" class="close" data-dismiss="modal" aria-label="Close"><span aria-hidden="true">&times;</span></button>
            <h4 class="modal-title">Reset Password</h4>
          </div>
          <form action="<c:url value="/reset/request"/>" method="POST">
            <input type="hidden" name="${_csrf.parameterName}" value="${_csrf.token}"/>
            <div class="modal-body">
              <p>Please enter your username below and click Submit. You will be emailed a link that will allow you to reset your password.</p>
              <div class="form-group">
                <label class="control-label required">Username</label>
                <input data-modal-focus type="text" class="form-control" data-required name="username"/>
              </div>
            </div>
            <div class="modal-footer">
              <button type="submit" class="btn btn-primary">Submit</button>
            </div>
          </form>
        </div>
      </div>
    </div>

  </jsp:attribute>
</t:page>