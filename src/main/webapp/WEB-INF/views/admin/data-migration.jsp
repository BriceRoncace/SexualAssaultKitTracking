<%@page contentType="text/html" pageEncoding="UTF-8"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="t" tagdir="/WEB-INF/tags" %>
<t:page>
  <jsp:attribute name="body">
    <%@include file="includes/admin-nav.jspf" %>
    <%@include file="../../includes/messages.jspf" %>
    <h2>Data Migration</h2>

    <div class="row">
      
      <div class="col-xs-12">
        <div class="panel panel-primary">
          <div class="panel-body">
            <p>To import organizations or users, please verify your CSV file matches the appropriate column configuration. All columns require a value except for True/False, where blank is considered false.</p>
          </div>
        </div>
      </div>
      
      <div class="col-xs-12">
        <div class="panel panel-primary">
          <div class="panel-heading">Organization</div>
          <div class="panel-body">
            <table class="table table-bordered">
              <thead>
                <tr>
                  <th colspan="3">Do not include passkeys. These will be automatically generated.</th>
                </tr>
              </thead>
              <tbody>
                <tr>
                  <td>1st Column</td>
                  <td>Name</td>
                  <td>Any <b>unique</b> string.</td>
                </tr>
                <tr>
                  <td>2nd Column</td>
                  <td>Type</td>
                  <td>Must be one of: <b>Lab, Law Enforcement, Medical,</b> or <b>Prosecutor</b>.</td>
                </tr>
                <tr>
                  <td>3rd Column</td>
                  <td>Jurisdiction</td>
                  <td>Must be a valid jurisdiction name. Click <a href="#jurisdictionList" data-toggle="modal">here</a> for a complete list.</td>
                </tr>
              </tbody>
            </table>
          </div>
          <div class="panel-footer">
            <form action="<c:url value='/orgImport'/>" method="POST" enctype="multipart/form-data">
              <input type="hidden" name="${_csrf.parameterName}" value="${_csrf.token}"/>
              <div class="input-group">
                <span class="input-group-btn">
                  <label class="btn btn-default btn-file">
                    Browse...<input name="file" type="file" class="hidden" data-fill-input="#orgFileName"/>
                  </label>
                </span>
                <input id="orgFileName" type="text" class="form-control" readonly="true"/>
                <span class="input-group-btn">
                  <button type="submit" class="btn btn-primary">Submit</button>
                </span>
              </div>
            </form>
          </div>
        </div>
      </div>

      <div class="col-xs-12">
        <div class="panel panel-primary">
          <div class="panel-heading">Organization Users</div>
          <div class="panel-body">
            <table class="table table-bordered">
              <thead>
              </thead>
              <tbody>
                <tr>
                  <td>1st Column</td>
                  <td>Organization Administrator</td>
                  <td>True or False, you can use <b>1</b>, <b>true</b>, or <b>yes</b> for true and <b>0</b>, <b>false</b>, <b>no</b>, or <b>&lt;blank&gt;</b> for false.</td>
                </tr>
                <tr>
                  <td>2nd Column</td>
                  <td>Organization Contact</td>
                  <td>True or False, you can use <b>1</b>, <b>true</b>, or <b>yes</b> for true and <b>0</b>, <b>false</b>, <b>no</b>, or <b>&lt;blank&gt;</b> for false.</td>
                </tr>
                <tr>
                  <td>3rd Column</td>
                  <td>Username</td>
                  <td>Any <b>unique</b> string.</td>
                </tr>
                <tr>
                  <td>4th Column</td>
                  <td>Display Name</td>
                  <td>Any string. Possibly seen by the public.</td>
                </tr>
                <tr>
                  <td>5th Column</td>
                  <td>Phone</td>
                  <td>Any string. Possibly seen by the public.</td>
                </tr>
                <tr>
                  <td>6th Column</td>
                  <td>Email</td>
                  <td>Must be a valid formatted email. Possibly seen by the public.</td>
                </tr>
                <tr>
                  <td>7th Column</td>
                  <td>Password</td>
                  <td>Passwords must be 8 or more characters long and contain at least 1 capital letter, 1 number, and 1 special character. </td>
                </tr>
                <tr>
                  <td>8th Column</td>
                  <td>Organization Name</td>
                  <td>String that matches the appropriate name of the Organization the user is attached to.</td>
                </tr>
              </tbody>
            </table>
          </div>
          <div class="panel-footer">
            <form action="<c:url value='/userImport'/>" method="POST" enctype="multipart/form-data">
              <input type="hidden" name="${_csrf.parameterName}" value="${_csrf.token}"/>
              <div class="input-group">
                <span class="input-group-btn">
                  <label class="btn btn-default btn-file">
                    Browse...<input name="file" type="file" class="hidden" data-fill-input="#userFileName"/>
                  </label>
                </span>
                <input id="userFileName" type="text" class="form-control" readonly="true"/>
                <span class="input-group-btn">
                  <button type="submit" class="btn btn-primary">Submit</button>
                </span>
              </div>
            </form>
          </div>
        </div>
      </div>

    </div>
              
    <div id="jurisdictionList" class="modal fade" tabindex="-1" role="dialog">
      <div class="modal-dialog modal-sm" role="document">
        <div class="modal-content">
          <div class="modal-header">
            <button type="button" class="close" data-dismiss="modal" aria-label="Close"><span aria-hidden="true">&times;</span></button>
            <h4 class="modal-title">Jurisdiction</h4>
          </div>
          <div class="modal-body" style="max-height: 500px; overflow: auto;">
            <c:forEach var="jurisdiction" items="${jurisdictions}">
              <p>${jurisdiction.name}</p>
            </c:forEach>
          </div>
        </div>
      </div>
    </div>

  </jsp:attribute>
  <jsp:attribute name="scripts">
    <script type="text/javascript">
      $(function() {
        $('[data-fill-input]').change(function () {
          $($(this).attr('data-fill-input')).val($(this).val());
        });
      });
    </script>
  </jsp:attribute>
</t:page>