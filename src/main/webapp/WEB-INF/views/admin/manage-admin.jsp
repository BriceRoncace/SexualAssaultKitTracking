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
    <t:breadcrumb crumbActive="Manage FS Admins"/>
    <h2>Manage Forensic Services Administrator Users</h2>

    <div class="col-sm-offset-3 col-sm-6">
      <div class="panel panel-primary">
        <div class="panel-heading">
          Forensic Services Administrators
          <small class="pull-right">
            <a class="btn btn-xs btn-info topNeg3" href="#userDialog" data-toggle="modal"><span class="glyphicon glyphicon-user"></span> New</a>
          </small>
        </div>
        <table class="table table-condensed">
          <tbody>
            <c:forEach var="adminUser" items="${adminUsers}">
              <tr>
                <td>${adminUser.displayName}</td>
                <td class="width20"><a href="javascript://nop" data-remove-admin="${adminUser.id}"><span class="glyphicon glyphicon-trash"></span></a></td>
              </tr>
            </c:forEach>
          </tbody>
        </table>
      </div>
    </div>

    <div class="modal fade" id="userDialog" tabindex="-1" role="dialog" aria-labelledby="myModalLabel">
      <div class="modal-dialog" role="document">
        <div class="modal-content">
          <div class="modal-header">
            <button type="button" class="close" data-dismiss="modal" aria-label="Close"><span aria-hidden="true">&times;</span></button>
            <h4 class="modal-title" id="myModalLabel">Add Administrator User</h4>
          </div>
          <form id="newUserForm" action="<c:url value="/adminUser/new"/>" method="POST">
            <input type="hidden" name="${_csrf.parameterName}" value="${_csrf.token}"/>
            <div class="modal-body">
              <div class="form-group">
                <label class="control-label">Find User by Last Name</label>
                <input id="lastNameInput" type="text" class="form-control" data-ldap-user-suggest data-modal-focus/>
                <input id="ldapUsername" type="hidden" name="ldapUsername"/>
              </div>
            </div>
            <div class="modal-footer">
              <button type="button" class="btn btn-default" data-dismiss="modal">Close</button>
              <button type="submit" class="btn btn-primary">Submit</button>
            </div>
          </form>
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
    <script src="<c:url value="/assets/js/ldap-user-suggest.js"/>" ></script>
    <script type="text/javascript">
      $(function () {
        ldapUserSuggest.setup({url: '<c:url value="/ldapUser/lastNameLike/%QUERY"/>', onSelect: ldapUserSelected});
        
        function ldapUserSelected(selection) {
          $('#ldapUsername').val(selection.username);
        }
        
        $('[data-remove-admin]').click(function () {
          $('#removeUserId').val($(this).attr('data-remove-admin'));
          $('#confirmRemoveMessage').text('Are you sure you would like to remove ' + $(this).closest('tr').first('td').text() + '?');
          $('#removeUserDialog').modal('show');
        });
      });
    </script>
  </jsp:attribute>
</t:page>