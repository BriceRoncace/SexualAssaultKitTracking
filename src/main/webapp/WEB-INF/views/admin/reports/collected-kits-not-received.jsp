<%@page contentType="text/html" pageEncoding="UTF-8"%>
<%@ taglib prefix="t" tagdir="/WEB-INF/tags" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>

<t:page>
  <jsp:attribute name="body">
    <%@include file="../includes/admin-nav.jspf" %>
    <%@include file="../../../includes/messages.jspf" %>
    <div class="panel panel-primary">
      <div class="panel-heading"><h2 class="panel-title">Kits Collected but not Received</h2></div>
      <div class="panel-body">
        <form method="GET">
          <div class="row hidden-print">
            <div class="col-xs-4">
              <div class="form-group">
                <label class="control-label">Jurisdiction</label>
                <t:select name="jurisdictionId" from="${jurisdictions}" emptyOption="" cssClass="form-control" optionKey="id" optionValue="displayName" value="${criteria.jurisdictionId}"/>
              </div>
            </div>
            <div class="col-xs-4">
              <div class="form-group">
                <label class="control-label">Requesting Agency</label>
                <t:select name="requestingLeAgencyId" from="${leOrganizations}" emptyOption="" cssClass="form-control" optionKey="id" optionValue="name" value="${criteria.requestingLeAgencyId}"/>
              </div>
            </div>
            <div class="col-xs-4 top25 text-center ">
              <button data-action="<c:url value="/report/collectedKitsNotReceived"/>" type="submit" class="btn btn-primary">Generate</button>
              <button data-action="<c:url value="/report/collectedKitsNotReceived/download"/>" type="submit" class="btn btn-default"><i class="fa fa-arrow-circle-o-down font18" title="download"></i> Export</button>
            </div>  
          </div>
          <div class="row hidden-print">
            <div class="col-xs-9 top15">
              <small><a href="javascript://nop" id="show-all">show all</a> / <a href="javascript://nop" id="hide-all">hide all</a></small>
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
              <th>Last Modified By</th>
              <th>Last Modified Date</th>
            </tr>
          </thead>
          <tbody>
            <c:forEach var="group" items="${report.groups}">
              <c:forEach var="row" items="${group.rows}">
                <tr class="kitRows hidden ${group.groupId}">
                  <td><c:out value="${row.name}"/></td>
                  <td><a href="<c:url value="/admin/view?id=${row.id}"/>">${row.serialNumber}</a></td>
                  <td>${row.lastModifiedBy}</td>
                  <td>${dateFormatter.format(row.lastModified)}</td>
                </tr>
              </c:forEach>
                <tr class="groupTotal" data-clickable="${group.groupId}">
                  <td><span>&nbsp;&nbsp;&nbsp;&nbsp;<c:out value="${group.agencyName}"/></span></td>
                  <td></td>
                  <td></td>
                  <td><span class="pull-right">Total: ${group.rows.size()} kits</span></td>
                </tr>
            </c:forEach>
            <tr style="font-weight: bold;">
              <td></td>
              <td></td>
              <td></td>
              <td class="text-right">Grand Total: ${report.kitSize} kits</td>
            </tr>
          </tbody>
          <tfoot>

          </tfoot>
        </table>
      </c:if>
    </div>
  </jsp:attribute>
  <jsp:attribute name="scripts">
    <script src="<c:url value="../assets/js/report-scripts.js"/>"></script>
  </jsp:attribute>
</t:page>