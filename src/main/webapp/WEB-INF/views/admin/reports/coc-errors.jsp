<%@page contentType="text/html" pageEncoding="UTF-8"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="t" tagdir="/WEB-INF/tags" %>
<%@ taglib prefix="cjisTags" uri="http://isp.idaho.gov/jsp/cjisTags"%>
<t:page>
  <jsp:attribute name="body">
    <%@include file="../includes/admin-nav.jspf" %>
    <%@include file="../../../includes/messages.jspf" %>
    <div class="panel panel-primary">
      <div class="panel-heading"><h2 class="panel-title">Kits with Questionable Timeline Events</h2></div>
      <div class="panel-body">
        <div class="row hidden-print">
          <div class="col-xs-2" style="margin-top: 19px;">
            <small><a href="javascript://nop" id="show-all">show all</a> / <a href="javascript://nop" id="hide-all">hide all</a></small>
          </div>
         <div class="col-xs-offset-8 col-xs-1 hidden-print text-right">
            <a href="<c:url value='/report/cocErrors/download'/>" data-export type="button" class="btn btn-default"><i class="fa fa-arrow-circle-o-down font18" title="download"></i> Export</a>
          </div>
        </div>
      </div>
      <c:if test="${not empty report}">
        <table class="table">
          <thead>
            <tr>
              <th>Agency</th>
              <th>Serial Number</th>
            </tr>
          </thead>
          <tbody>
            <c:forEach var="group" items="${report.groups}">
              <c:forEach var="row" items="${group.rows}">
                <tr class="kitRows hidden ${group.groupId}">
                  <td><c:out value="${row.name}"/></td>
                  <td><a href="<c:url value="/admin/view?id=${row.id}"/>">${row.serialNumber}</a></td>
                </tr>
              </c:forEach>
                <tr class="groupTotal" data-clickable="${group.groupId}">
                  <td>&nbsp;&nbsp;&nbsp;&nbsp;<c:out value="${group.agencyName}"/></td>
                  <td class="text-right">Total: ${group.rows.size()} kits</td>
                </tr>
            </c:forEach>
            <tr style="font-weight: bold;">
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
    <script src="<c:url value="../assets/js/saktrack-search.js"/>"></script>
    <script src="<c:url value="../assets/js/report-scripts.js"/>"></script>
  </jsp:attribute>
</t:page>