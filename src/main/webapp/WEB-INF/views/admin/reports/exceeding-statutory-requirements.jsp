<%@page contentType="text/html" pageEncoding="UTF-8"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="t" tagdir="/WEB-INF/tags" %>
<%@ taglib prefix="cjisTags" uri="http://isp.idaho.gov/jsp/cjisTags"%>
<t:page>
  <jsp:attribute name="body">
    <%@include file="../includes/admin-nav.jspf" %>
    <%@include file="../../../includes/messages.jspf" %>
    <div class="panel panel-primary">
      <div class="panel-heading"><h2 class="panel-title">Kits Exceeding Statutory Requirements</h2></div>
      <div class="panel-body">
        <form method="GET">
          <div class="row hidden-print">
            <div class="col-xs-4">
              <div class="form-group">
                <label class="control-label">Jurisdiction</label>
                <cjisTags:select name="jurisdictionId" from="${jurisdictions}" emptyOption="" cssClass="form-control" optionKey="id" optionValue="displayName" value="${criteria.jurisdictionId}"/>
              </div>
            </div>
            <div class="col-xs-4">
              <div class="form-group">
                <label class="control-label">Requesting Agency</label>
                <cjisTags:select name="requestingLeAgencyId" from="${leOrganizations}" emptyOption="" cssClass="form-control" optionKey="id" optionValue="name" value="${criteria.requestingLeAgencyId}"/>
              </div>
            </div>
          </div>
          <div class="row hidden-print">
            <div class="col-xs-9 top15">
              <small><a href="javascript://nop" id="show-all">show all</a> / <a href="javascript://nop" id="hide-all">hide all</a></small>
            </div>
            <div class="col-xs-3 text-right">
              <button data-action="<c:url value="/report/exceedingStatutoryRequirements/download"/>" type="submit" class="btn btn-default"><i class="fa fa-arrow-circle-o-down font18" title="download"></i> Export</button>
              <button data-action="<c:url value="/report/exceedingStatutoryRequirements"/>" type="submit" class="btn btn-primary">Generate</button>
            </div>
          </div>
          <div class="row visible-print">
            <div class="col-xs-12 small">
              <c:out value="${filterText}"/>
            </div>
          </div>
        </form>
      </div>
      <c:if test="${not empty report}">
        <table class="table">
          <thead>
            <tr>
              <th>Law Enforcement Agency</th>
              <th>Serial Number</th>
              <th>Number of Days</th>
            </tr>
          </thead>
          <tbody>
            <c:forEach var="group" items="${report.groups}">
              <c:if test="${group.rows.size() > 0}">
                <c:forEach var="row" items="${group.rows}">
                  <tr class="kitRows hidden ${group.groupId}">
                    <td><c:out value="${row.name}"/></td>
                    <td><a href="<c:url value="/admin/view?id=${row.id}"/>">${row.serialNumber}</a></td>
                    <td>${row.days}</td>
                  </tr>
                </c:forEach>
                  <tr class="groupTotal" data-clickable="${group.groupId}">
                    <td><span>&nbsp;&nbsp;&nbsp;&nbsp;<c:out value="${group.requirement}"/></span><span class="pull-right">Total: ${group.rows.size()} kits</span></td>
                    <td class="text-right">Average:</td>
                    <td>${group.averageDays}</td>
                  </tr>
                </c:if>
            </c:forEach>
          </tbody>
          <tfoot>

          </tfoot>
        </table>
      </c:if>
    </div>
  </jsp:attribute>
  <jsp:attribute name="scripts">
    <script src="<c:url value="../assets/js/saktrack-search.js"/>"></script>
    <script src="<c:url value="../assets/js/report-scripts.js"/>"></script>
  </jsp:attribute>
</t:page>