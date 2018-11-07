<%@page contentType="text/html" pageEncoding="UTF-8"%>
<%@ taglib prefix="t" tagdir="/WEB-INF/tags" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>

<t:page>
  <jsp:attribute name="head">
    <link href="<c:url value="/assets/css/pagination.css"/>" rel="stylesheet" type="text/css" />
  </jsp:attribute>
  <jsp:attribute name="body">
    <%@include file="includes/admin-nav.jspf" %>
    <%@include file="../../includes/messages.jspf" %>
    <h2><c:out value="${user.displayName}"/> Dashboard</h2>

    <c:if test="${not empty kitsMissingEvents}">
      <div class="row">
        <div class="col-sm-12">
          <div class="panel panel-primary">
            <div class="panel-heading">Kits with Questionable Events</div>
            <table class="table table-condensed table-hover">
              <tbody>
                <c:forEach var="kit" items="${kitsMissingEvents}">
                  <tr class="clickable" data-href="<c:url value="/admin/manageEvents"/>?kitId=${kit.id}">
                    <td>${kit.serialNumber}</td>
                    <td><c:out value="${kit.currentCustody.prettyPrint(true)}"/></td>
                  </tr>
                </c:forEach>
              </tbody>
            </table>
          </div>
        </div>
      </div>
    </c:if>

    <div class="row">
      <div class="col-sm-12">
        <div class="panel panel-primary">
          <div class="panel-heading">
            Organizations
            <small class="pull-right">
              <button id="filterOpen" class="btn btn-xs btn-info topNeg3 filter-open <c:if test="${!criteria.isEmpty()}">hidden</c:if>" data-show=".filter-closed" data-hide=".filter-open"><span class="glyphicon glyphicon-filter"></span> Filter <span class="glyphicon glyphicon-arrow-down"></span></button>
              <button id="filterClose" class="btn btn-xs btn-info topNeg3 filter-closed <c:if test="${criteria.isEmpty()}">hidden</c:if>" data-show=".filter-open" data-hide=".filter-closed"><span class="glyphicon glyphicon-filter"></span> Filter <span class="glyphicon glyphicon-arrow-up"></span></button>
              <a class="btn btn-xs btn-info topNeg3" href="<c:url value="/organization/new"/>"><span class="glyphicon glyphicon-home"></span> New</a>
            </small>
          </div>
          <div id="filterDiv" class="panel-body filter-closed <c:if test="${criteria.isEmpty()}">hidden</c:if>">
            <form id="filter-form" action="<c:url value='/admin/dashboard'/>" method="GET">
              <div class="row">
                <div class="form-group col-xs-5">
                  <label class="control-label">Name</label>
                  <input type="text" class="form-control" name="name" value="${criteria.name}"/>
                </div>
                <div class="form-group col-xs-3">
                  <label class="control-label">Type</label>
                  <t:select id="orgTypeSelect" from="${orgTypes}" name="type" cssClass="form-control" emptyOption="" optionValue="label" value="${criteria.type}"/>
                </div>
                <div class="form-group col-xs-3">
                  <label class="control-label">Jurisdiction</label>
                  <t:select id="orgJurisdictionsSelect" from="${jurisdictions}" name="jurisdictionId" cssClass="form-control" emptyOption="" optionKey="id" optionValue="displayName" value="${criteria.jurisdictionId}"/>
                </div>
                <div class="col-xs-1 top25 leftNeg10">
                  <button class="btn btn-default" type="submit">Update</button>
                </div>
              </div>
            </form>
          </div>
          <table class="table table-condensed table-hover">
            <thead>
              <tr>
                <th><t:columnsort sort="${page.sort}" propertyName="name" text="Name"/></th>
                <th><t:columnsort sort="${page.sort}" propertyName="type" text="Type"/></th>
                <th><t:columnsort sort="${page.sort}" propertyName="jurisdiction.name" text="Jurisdiction"/></th>
                <th class="noSort"></th>
              </tr>
            </thead>
            <tbody id="orgList">
              <c:forEach var="org" items="${page.content}">
                <tr class="${org.enabled ? '' : 'disabled'}" data-org-type="${org.type}" data-org-jurisdiction="${org.jurisdiction.id}">
                  <td>
                    <c:if test="${!org.enabled}"><span class="glyphicon glyphicon-ban-circle hover-pointer med-blue-text" data-toggle="tooltip" title="Disabled"></span>&nbsp;&nbsp;</c:if>
                    <a href="<c:url value="/${org.type.newUser.type.label}/dashboard?orgId=${org.id}"/>"><c:out value="${org.name}"/></a>
                  </td>
                  <td>${org.type.label}</td>
                  <td>${org.jurisdiction.displayName}</td>
                  <td style="width: 5%;">
                    <div class="dropdown">
                      <a href="javascrpt://nop/" data-toggle="dropdown"><span class="font18 glyphicon glyphicon-option-horizontal"></span></a>
                      <ul class="dropdown-menu dropdown-menu-right">
                        <li><a href="<c:url value="/${org.type.newUser.type.label}/dashboard?orgId=${org.id}"/>"><span class="glyphicon glyphicon-dashboard"></span> Dashboard</a></li>
                        <li><a href="<c:url value="/organization/${org.id}"/>"><span class="glyphicon glyphicon-pencil"></span> Manage</a></li>
                        <li>
                          <a href="javascript://nop/">
                            <form class="confirmation" action="<c:url value="/organization/${org.id}/remove"/>" data-confirmation-label="Delete Organization?" method="POST">
                              <input type="hidden" name="${_csrf.parameterName}" value="${_csrf.token}"/>
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

          <c:if test="${page.size < page.totalElements}">
            <div class="col-sm-12 top5">
              <t:pager page="${page}" showInfo="true"/>
            </div>
          </c:if>

        </div>
      </div>
    </div>
  </jsp:attribute>
  <jsp:attribute name="scripts">
    <script type="text/javascript">

    </script>
  </jsp:attribute>
</t:page>
