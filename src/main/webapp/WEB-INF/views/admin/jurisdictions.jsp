<%@page contentType="text/html" pageEncoding="UTF-8"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="cjisTags" uri="http://isp.idaho.gov/jsp/cjisTags"%>
<%@ taglib prefix="t" tagdir="/WEB-INF/tags" %>
<t:page>
  <jsp:attribute name="head">

  </jsp:attribute>
  <jsp:attribute name="body">
    <%@include file="includes/admin-nav.jspf" %>
    <%@include file="../../includes/messages.jspf" %>
  
    <h2>Jurisdictions</h2>
    
    <div class="row">
      <div class="col-sm-6">
        <div class="panel panel-primary">
          <div class="panel-heading">Jurisdictions</div>
          <table class="table table-condensed table-hover">
            <thead>
              <tr>
                <th>Name</th>
                <th>Type</th>
                <th></th>
              </tr>
            </thead>
            <tbody>
              <c:forEach var="jurisdiction" items="${jurisdictions}">
                <tr>
                  <td><c:out value="${jurisdiction.name}"/></td>
                  <td>${jurisdiction.type.label}</td>
                  <td style="width: 5%;">
                    <div class="dropdown">
                      <a href="javascrpt://nop/" data-toggle="dropdown"><span class="font18 glyphicon glyphicon-option-horizontal"></span></a>
                      <ul class="dropdown-menu dropdown-menu-right">
                        <li><a href="<c:url value="/jurisdiction/${jurisdiction.id}"/>"><span class="glyphicon glyphicon-pencil"></span> Edit</a></li>
                        <li>
                          <a href="javascript://nop/">
                            <form class="confirmation" action="<c:url value="/jurisdiction/remove"/>" data-confirmation-label="Delete Jurisdiction?" method="POST">
                              <input type="hidden" name="${_csrf.parameterName}" value="${_csrf.token}"/>
                              <input type="hidden" name="jurisdictionId" value="${jurisdiction.id}"/>
                              <button type="submit" class="btn btn-link no-margin no-padding no-text-decoration font12 red-text"><span class="glyphicon glyphicon-remove"></span> Delete</button>
                            </form>
                          </a>
                        </li>
                      </ul>
                    </div>
                  </td>
                </tr>
              </c:forEach>
            </tbody>
          </table>
        </div>
      </div>
      <div class="col-sm-6">
        <form action="<c:url value="/jurisdiction/save"/>" method="POST">
          <input type="hidden" name="${_csrf.parameterName}" value="${_csrf.token}"/>
          <input type="hidden" name="jurisdictionId" value="${jurisdiction.id}"/>
          <div class="panel panel-primary">
            <div class="panel-heading">Jurisdiction</div>
            <div class="panel-body row">
              <div class="col-xs-7">
                <div class="form-group">
                  <label class="control-label required">Name</label>
                  <input type="text" class="form-control" name="name" data-required data-focus-if-empty value="${jurisdiction.name}"/>
                </div>
              </div>
              <div class="col-xs-5">
                <div class="form-group">
                  <label class="control-label required">Type</label>
                <cjisTags:select name="type" from="${jurisdictionTypes}" emptyOption="" cssClass="form-control" optionValue="label" value="${jurisdiction.type}"/>
                </div>
              </div>
            </div>
            <div class="panel-footer text-right">
              <a href="<c:url value="/jurisdictions/list"/>" class="btn btn-default">Cancel</a>
              <button type="submit" class="btn btn-primary">${jurisdiction.id != null ? 'Update' : 'Save'}</button>
            </div>
          </div>
        </form>
      </div>
    </div>
  </jsp:attribute>
</t:page>