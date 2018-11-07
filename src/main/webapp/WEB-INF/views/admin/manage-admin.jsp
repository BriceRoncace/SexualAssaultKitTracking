<%@page contentType="text/html" pageEncoding="UTF-8"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="t" tagdir="/WEB-INF/tags" %>
<t:page>
  <jsp:attribute name="css">
    <link href="<c:url value="/assets/css/typeahead.js-bootstrap.css"/>" rel="stylesheet" type="text/css" />
  </jsp:attribute>
  <jsp:attribute name="body">
    <%@include file="includes/admin-nav.jspf" %>
    <%@include file="../../includes/messages.jspf" %>
    <t:breadcrumb crumbActive="Manage Admins"/>
    <h2>Manage Administrators</h2>

    <div class="row">
      <div class="col-sm-12">
        <div class="panel panel-primary">
          <div class="panel-heading">
            Administrators
            <small class="pull-right">
              <a class="btn btn-xs btn-info topNeg3" href="<c:url value="/adminUser/save"/>"><span class="glyphicon glyphicon-user"></span> New</a>
            </small>
          </div>
          <table class="table table-condensed table-hover">
            <tbody>
              <c:forEach var="adminUser" items="${adminUsers}">
                <tr class="${adminUser.enabled ? '' : 'disabled'}">
                  <td class="clickable" data-href="<c:url value="/adminUser/save?userId=${user.id}"/>">${adminUser.displayName}<c:if test="${!adminUser.enabled}">&nbsp;&nbsp;<small><span class="glyphicon glyphicon-ban-circle hover-pointer med-blue-text" data-toggle="tooltip" title="User Disabled"></span></small></c:if></td>
                  <td class="width20"><a href="javascript://nop" data-remove-admin="${adminUser.id}"><span class="glyphicon glyphicon-trash"></span></a></td>
                </tr>
              </c:forEach>
            </tbody>
          </table>
        </div>
      </div>
    </div>
    
    <div class="modal fade" id="removeUserDialog" tabindex="-1" role="dialog" aria-labelledby="myModalLabel">
      <div class="modal-dialog" role="document">
        <div class="modal-content">
          <div class="modal-header">
            <button type="button" class="close" data-dismiss="modal" aria-label="Close"><span aria-hidden="true">&times;</span></button>
            <h4 class="modal-title" id="myModalLabel">Remove Administrator User</h4>
          </div>
          <form id="removeUserForm" action="<c:url value="/adminUser/remove"/>" method="POST">
            <input type="hidden" name="${_csrf.parameterName}" value="${_csrf.token}"/>
            <input id="removeUserId" type="hidden" name="userId"/>
            <div id="confirmRemoveMessage" class="modal-body"></div>
            <div class="modal-footer">
              <button class="btn btn-danger">Yes</button>
              <button type="button" class="btn btn-default" data-dismiss="modal">No</button>
            </div>
          </form>
        </div>
      </div>
    </div>

  </jsp:attribute>
  <jsp:attribute name="scripts">
    <script src="<c:url value="/assets/js/typeahead.min.js"/>" ></script>
    <script src="<c:url value="/assets/js/bloodhound.min.js"/>" ></script>
    <script type="text/javascript">
      $(function () {
        $('[data-remove-admin]').click(function () {
          $('#removeUserId').val($(this).attr('data-remove-admin'));
          $('#confirmRemoveMessage').text('Are you sure you would like to remove ' + $(this).closest('tr').first('td').text() + '?');
          $('#removeUserDialog').modal('show');
        });
      });
    </script>
  </jsp:attribute>
</t:page>