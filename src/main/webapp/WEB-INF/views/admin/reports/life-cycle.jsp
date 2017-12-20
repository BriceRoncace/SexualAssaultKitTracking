<%@page contentType="text/html" pageEncoding="UTF-8"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="t" tagdir="/WEB-INF/tags" %>
<%@ taglib prefix="cjisTags" uri="http://isp.idaho.gov/jsp/cjisTags"%>
<t:page>
  <jsp:attribute name="body">
    <%@include file="../includes/admin-nav.jspf" %>
    <%@include file="../../../includes/messages.jspf" %>
    <div class="panel panel-primary">
      <div class="panel-heading"><h2 class="panel-title">Life Cycle Report</h2></div>
      <div class="panel-body">
        <form method="GET">
          <div class="row hidden-print">
            <div class="col-xs-4">
              <div class="form-group">
                <label class="control-label">Collected Date</label>
                <span class="clickable label thin label-primary" data-search-type="ON">On</span>
                <span class="clickable label thin label-default clickable" data-search-type="BEFORE">Before</span>
                <span class="clickable label thin label-default" data-search-type="AFTER">After</span>
                <span class="clickable label thin label-default" data-search-type="BETWEEN">Between</span>
                <input type="hidden" name="collectedDate.searchType" value="${criteria.collectedDate.searchType == null ? 'ON' : criteria.collectedDate.searchType}"/>
                <div class="input-group">
                  <input type="text" class="form-control hasDatePicker" name="collectedDate.date1" value="<cjisTags:fmtTemporal value="${criteria.collectedDate.date1}" pattern="${dateFormat}"/>">
                  <span class="input-group-addon invisible">-</span>
                  <input type="text" class="form-control hasDatePicker invisible" name="collectedDate.date2" value="<cjisTags:fmtTemporal value="${criteria.collectedDate.date2}" pattern="${dateFormat}"/>">
                </div>
              </div>
            </div>
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
              
          <div class="row">
            <div class="col-xs-9">
              <div class="col-xs-12 hidden-print">
                <small><a href="javascript://nop" id="show-all">show all</a> / <a href="javascript://nop" id="hide-all">hide all</a></small>
              </div>
              <div class="col-xs-12 visible-print small">
                <c:out value="${filterText}"/>
              </div>
              <div class="col-xs-12 small">* - Kit is currently at location.</div>
            </div>
            <div class="col-xs-3 hidden-print text-right">
              <button data-action="<c:url value="/report/lifeCycle/download"/>" type="submit" class="btn btn-default"><i class="fa fa-arrow-circle-o-down font18" title="download"></i> Export</button>
              <button data-action="<c:url value="/report/lifeCycle"/>" type="submit" class="btn btn-primary">Generate</button>
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
              <th>Days at Medical</th>
              <th>Days in Transit</th>
              <th>Days at Law Enforcement</th>
              <th>Days in Analysis</th>
            </tr>
          </thead>
          <tbody>
            <c:forEach var="group" items="${report.groups}">
              <c:forEach var="row" items="${group.rows}">
                <tr class="kitRows hidden ${group.groupId}">
                  <td><c:out value="${row.name}"/></td>
                  <td><a href="<c:url value="/admin/view?id=${row.id}"/>">${row.serialNumber}</a></td>
                  <td>${row.timeAtMedical}${row.isAtMedical() ? "*" : ""}</td>
                  <td>${row.timeInTransit}${row.isInTransit() ? "*" : ""}</td>
                  <td>${row.timeAtLawEnforcement}${row.isAtLawEnforcement() ? "*" : ""}</td>
                  <td>${row.timeAtLab}${row.isAtLab() ? "*" : ""}</td>
                </tr>
              </c:forEach>
                <tr class="groupTotal" data-clickable="${group.groupId}">
                  <td><span>&nbsp;&nbsp;&nbsp;&nbsp;<c:out value="${group.agencyName}"/></span><span class="pull-right">Total: ${group.rows.size()} kits</span></td>
                  <td class="text-right">Average:</td>
                  <td>${group.timeAtMedicalAverage}</td>
                  <td>${group.timeInTransitAverage}</td>
                  <td>${group.timeAtLawEnforcementAverage}</td>
                  <td>${group.timeAtLabAverage}</td>
                </tr>
            </c:forEach>
            <tr style="font-weight: bold;">
              <td class="text-right">Grand Total: ${report.kitSize} kits</td>
              <td class="text-right">Average:</td>
              <td>${report.timeAtMedicalAverage}</td>
              <td>${report.timeInTransitAverage}</td>
              <td>${report.timeAtLawEnforcementAverage}</td>
              <td>${report.timeAtLabAverage}</td>
            </tr>
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