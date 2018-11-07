<%@page contentType="text/html" pageEncoding="UTF-8"%>
<%@ taglib prefix="t" tagdir="/WEB-INF/tags" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>

<t:page>
  <jsp:attribute name="body">
    <%@include file="../includes/admin-nav.jspf" %>
    <%@include file="../../../includes/messages.jspf" %>
    <div class="panel panel-primary hidden-print">
      <div class="panel-heading"><h2 class="panel-title">Date Range Report</h2></div>
      <div class="panel-body">
        <form action="<c:url value="/report/timeframe"/>" method="POST">
          <input type="hidden" name="${_csrf.parameterName}" value="${_csrf.token}"/>
          <div class="row">
            <div class="col-xs-4">
              <div class="form-group">
                <label class="control-label">Start Date</label>
                <input type="text" class="form-control hasDatePicker" name="start" value="${dateFormatter.format(startDate)}">
              </div>
            </div>
            <div class="col-xs-4">
              <div class="form-group">
                <label class="control-label">End Date</label>
                <input type="text" class="form-control hasDatePicker" name="end" value="${dateFormatter.format(endDate)}">
              </div>
            </div>
            <div class="col-xs-2 pull-right top20">
              <button type="submit" class="btn btn-primary">Generate</button>
            </div>
          </div>
        </form>
      </div>
    </div>
              
              
    <c:if test="${distributedKitsReport.kitSize > 0}">
      <h2>Report for ${dateFormatter.format(startDate)} through ${dateFormatter.format(endDate)}</h2>

      
      <div class="panel panel-primary">
        <div class="panel-heading">
          <h3 class="panel-title">Summary</h3>
        </div>
        <div class="panel-body">
          <table class="table table-striped">
            <tbody>
              <tr><td><b>New Distributed Kits</b></td><td><b>${distributedKitsReport.kitSize}</b></td></tr>
              <tr><td><b>Collected Kits received by Requesting Law Enforcement Agency</b></td><td><b>${receivedFromMedicalReport.kitSize}</b></td></tr>
              <tr><td><b>Kits NOT Submitted to Lab</b></td><td><b>${unsubmittableKitsSize}</b></td></tr>
              <tr><td style="padding-left: 27px;">There is no evidence to support a crime being committed.</td><td>${noEvidenceReport.kitSize}</td></tr>
              <tr><td style="padding-left: 27px;">It is no longer being investigated as a crime.</td><td>${notACrimeReport.kitSize}</td></tr>
              <tr><td style="padding-left: 27px;">An adult victim expressly indicates that no further forensic examination or testing occur.</td><td>${noTestingReport.kitSize}</td></tr>
              <tr><td><b>Kits Sent to Lab</b></td><td><b>${submittedKitsReport.kitSize}</b></td></tr>
              <tr><td><b>Profiles Entered into DNA Database</b></td><td><b>${databaseKitsReport.kitSize}</b></td></tr>
              <tr><td><b>DNA Database Hits</b></td><td><b>${hitsInKitsReport.kitSize}</b></td></tr>
            </tbody>
          </table>
        </div>
      </div>      
      

      <div class="panel panel-primary">
        <div class="panel-heading">
          <h3 class="panel-title">Details <small class="pull-right top5"><a href="javascript://nop" id="show-all">show all</a> / <a href="javascript://nop" id="hide-all">hide all</a></small></h3>
        </div>
        <div class="panel-body">

          <div class="well">
            
            <h4>
              <a href="<c:url value="/report/distributedKitsReport/download"/>?start=${dateFormatter.format(startDate)}&end=${dateFormatter.format(endDate)}"><i class="fa fa-arrow-circle-o-down font18" title="download"></i></a>&nbsp;
              <b>New Distributed Kits</b>
              <span class="pull-right"><small>Date new kit sent from lab is between ${dateFormatter.format(startDate)} and ${dateFormatter.format(endDate)}</small></span>
            </h4>
            <c:forEach var="group" items="${distributedKitsReport.groups}">
              <table class="table">
                <tr class="groupTotal" data-clickable="dist-${group.groupId}">
                  <td style="width: 5%;"><span id="dist-${group.groupId}" class="glyphicon glyphicon-menu-down" aria-hidden="true" data-toggle="tooltip" data-placement="top" title="Details"></span></td>
                  <td style="width: 45%;"><b>${group.agencyName}</b></td>
                  <td style="width: 50%;"><b>${group.rows.size()}</b> kits</td>
                </tr>
              </table>
              <table class="table table-striped kitRows hidden dist-${group.groupId}">
                <thead>
                  <tr>
                    <th style="width: 5%;"></th>
                    <th style="width: 45%;">Serial Number</th>
                    <th style="width: 50%;">Expiration Date</th>
                  </tr>
                </thead>
                <tbody>
                  <c:forEach var="row" items="${group.rows}">
                    <tr>
                      <td style="width: 5%;"></td>
                      <td style="width: 45%;"><a href="<c:url value="/admin/view?id=${row.id}"/>">${row.serialNumber}</a></td>
                      <td style="width: 50%;">${dateFormatter.format(row.expirationDate)}</td>
                    </tr>
                  </c:forEach>
                </tbody>
                <tfoot>
                </tfoot>
              </table>
            </c:forEach>
          </div>
            
          <div class="well">
            <h4>
              <a href="<c:url value="/report/receivedCollectedKitsReport/download"/>?start=${dateFormatter.format(startDate)}&end=${dateFormatter.format(endDate)}"><i class="fa fa-arrow-circle-o-down font18" title="download"></i></a>&nbsp;
              <b>Collected Kits received by Requesting Law Enf. Agency</b><span class="pull-right"><small>Date collected kit received from medical is between ${dateFormatter.format(startDate)} and ${dateFormatter.format(endDate)}</small></span>
            </h4>
            <c:forEach var="group" items="${receivedFromMedicalReport.groups}">
              <table class="table">
                <tr class="groupTotal" data-clickable="coll-${group.groupId}">
                  <td style="width: 5%;"><span id="coll-${group.groupId}" class="glyphicon glyphicon-menu-down" aria-hidden="true" data-toggle="tooltip" data-placement="top" title="Details"></span></td>
                  <td style="width: 45%;"><b>${group.agencyName}</b></td>
                  <td style="width: 50%;"><b>${group.rows.size()}</b> kits</td>
                </tr>
              </table>
              <table class="table table-striped kitRows hidden coll-${group.groupId}">
                <thead>
                  <tr>
                    <th style="width: 5%;"></th>
                    <th style="width: 45%;">Serial Number</th>
                    <th style="width: 20%;">Collection Date</th>
                    <th style="width: 30%;">Days At Medical</th>
                  </tr>
                </thead>
                <tbody>
                  <c:forEach var="row" items="${group.rows}">
                    <tr>
                      <td style="width: 5%;"></td>
                      <td style="width: 45%;"><a href="<c:url value="/admin/view?id=${row.id}"/>">${row.serialNumber}</a></td>
                      <td style="width: 20%;">${dateFormatter.format(row.collectionDate)}</td>
                      <td style="width: 30%;">${row.timeAtMedical}</td>
                    </tr>
                  </c:forEach>
                </tbody>
                <tfoot>
                </tfoot>
              </table>
            </c:forEach>
          </div>
            
            
          <div class="well">
            <h4>
              <a href="<c:url value="/report/unsubmittableKitsReport/download"/>?start=${dateFormatter.format(startDate)}&end=${dateFormatter.format(endDate)}"><i class="fa fa-arrow-circle-o-down font18" title="download"></i></a>&nbsp;
              <b>Kits NOT Submitted to Lab</b><span class="pull-right"><small>Date Attorney confirmed decision to NOT submit is between ${dateFormatter.format(startDate)} and ${dateFormatter.format(endDate)}</small></span>
            </h4>
            <c:if test="${noEvidenceReport.kitSize < 1 && notACrimeReport.kitSize < 1 && noTestingReport.kitSize < 1}">There are no kits in this date range or all were submitted to the lab</c:if>
            
            <c:if test="${noEvidenceReport.kitSize > 0}">
              <div class="left30">
                <h5><b>There is no evidence to support a crime being committed.</b></h5>
                <c:forEach var="group" items="${noEvidenceReport.groups}">
                  <table class="table">
                    <tr class="groupTotal" data-clickable="no-evid-${group.groupId}">
                      <td style="width: 5%;"><span id="no-evid-${group.groupId}" class="glyphicon glyphicon-menu-down" aria-hidden="true" data-toggle="tooltip" data-placement="top" title="Details"></span></td>
                      <td style="width: 45%;"><b><c:out value="${group.agencyName}"/></b></td>
                      <td style="width: 50%;"><b>${group.rows.size()}</b> kits</td>
                    </tr>
                  </table>
                  <table class="table table-striped kitRows hidden no-evid-${group.groupId}">
                    <thead>
                      <tr>
                        <th style="width: 5%;"></th>
                        <th style="width: 45%;">Serial Number</th>
                        <th style="width: 50%;">Prosecutor Notes</th>
                      </tr>
                    </thead>
                    <tbody>
                      <c:forEach var="row" items="${group.rows}">
                        <tr>
                          <td style="width: 5%;"></td>
                          <td style="width: 45%;"><a href="<c:url value="/admin/view?id=${row.id}"/>">${row.serialNumber}</a></td>
                          <td style="width: 50%;"><c:out value="${row.prosecutorNotes}"/></td>
                        </tr>
                      </c:forEach>
                    </tbody>
                    <tfoot>
                    </tfoot>
                  </table>
                </c:forEach>
              </div>
            </c:if>
            
            <c:if test="${notACrimeReport.kitSize > 0}">
              <div class="left30">
                <h5><b>No longer being investigated as a crime.</b></h5>
                <c:forEach var="group" items="${notACrimeReport.groups}">
                  <table class="table">
                    <tr class="groupTotal" data-clickable="no-crime-${group.groupId}">
                      <td style="width: 5%;"><span id="no-crime-${group.groupId}" class="glyphicon glyphicon-menu-down" aria-hidden="true" data-toggle="tooltip" data-placement="top" title="Details"></span></td>
                      <td style="width: 45%;"><b><c:out value="${group.agencyName}"/></b></td>
                      <td style="width: 50%;"><b>${group.rows.size()}</b> kits</td>
                    </tr>
                  </table>
                  <table class="table table-striped kitRows hidden no-crime-${group.groupId}">
                    <thead>
                      <tr>
                        <th style="width: 5%;"></th>
                        <th style="width: 45%;">Serial Number</th>
                        <th style="width: 50%;">Prosecutor Notes</th>
                      </tr>
                    </thead>
                    <tbody>
                      <c:forEach var="row" items="${group.rows}">
                        <tr>
                          <td style="width: 5%;"></td>
                          <td style="width: 45%;"><a href="<c:url value="/admin/view?id=${row.id}"/>">${row.serialNumber}</a></td>
                          <td style="width: 50%;"><c:out value="${row.prosecutorNotes}"/></td>
                        </tr>
                      </c:forEach>
                    </tbody>
                    <tfoot>
                    </tfoot>
                  </table>
                </c:forEach>
              </div>
            </c:if>
            
            <c:if test="${noTestingReport.kitSize > 0}">
              <div class="left30">
                <h5><b>Adult victim expressly indicates that no further forensic examination or testing occur.</b></h5>
                <c:forEach var="group" items="${noTestingReport.groups}">
                  <table class="table">
                    <tr class="groupTotal" data-clickable="no-test-${group.groupId}">
                      <td style="width: 5%;"><span id="no-test-${group.groupId}" class="glyphicon glyphicon-menu-down" aria-hidden="true" data-toggle="tooltip" data-placement="top" title="Details"></span></td>
                      <td style="width: 45%;"><b><c:out value="${group.agencyName}"/></b></td>
                      <td style="width: 50%;"><b>${group.rows.size()}</b> kits</td>
                    </tr>
                  </table>
                  <table class="table table-striped kitRows hidden no-test-${group.groupId}">
                    <thead>
                      <tr>
                        <th style="width: 5%;"></th>
                        <th style="width: 45%;">Serial Number</th>
                        <th style="width: 50%;">Prosecutor Notes</th>
                      </tr>
                    </thead>
                    <tbody>
                      <c:forEach var="row" items="${group.rows}">
                        <tr>
                          <td style="width: 5%;"></td>
                          <td style="width: 45%;"><a href="<c:url value="/admin/view?id=${row.id}"/>">${row.serialNumber}</a></td>
                          <td style="width: 50%;"><c:out value="${row.prosecutorNotes}"/></td>
                        </tr>
                      </c:forEach>
                    </tbody>
                    <tfoot>
                    </tfoot>
                  </table>
                </c:forEach>
              </div>
            </c:if>
          </div>

          <div class="well">
            <h4>
              <a href="<c:url value="/report/submittedKitsReport/download"/>?start=${dateFormatter.format(startDate)}&end=${dateFormatter.format(endDate)}"><i class="fa fa-arrow-circle-o-down font18" title="download"></i></a>&nbsp;
              <b>Kits Sent to Lab</b>
              <span class="pull-right"><small>Date Law Enforcement sent kit to lab is between ${dateFormatter.format(startDate)} and ${dateFormatter.format(endDate)}</small></span>
            </h4>
            <c:forEach var="group" items="${submittedKitsReport.groups}">
              <table class="table">
                <tr class="groupTotal" data-clickable="lab-${group.groupId}">
                  <td style="width: 5%;"><span id="lab-${group.groupId}" class="glyphicon glyphicon-menu-down" aria-hidden="true" data-toggle="tooltip" data-placement="top" title="Details"></span></td>
                  <td style="width: 45%;"><b><c:out value="${group.agencyName}"/></b></td>
                  <td style="width: 50%;"><b>${group.rows.size()}</b> kits</td>
                </tr>
              </table>
              <table class="table table-striped kitRows hidden lab-${group.groupId}">
                <thead>
                  <tr>
                    <th style="width: 5%;"></th>
                    <th style="width: 45%;">Serial Number</th>
                    <th style="width: 50%;">Date Received at Lab</th>
                  </tr>
                </thead>
                <tbody>
                    <c:forEach var="row" items="${group.rows}">
                      <tr>
                        <td style="width: 5%;"></td>
                        <td style="width: 45%;"><a href="<c:url value="/admin/view?id=${row.id}"/>">${row.serialNumber}</a></td>
                        <td style="width: 50%;">
                          <c:forEach var="date" items="${row.labReceivedDates}" varStatus="s">
                            ${dateFormatter.format(date)}<c:if test="${!s.last}">,&nbsp;</c:if>
                          </c:forEach>
                        </td>
                      </tr>
                    </c:forEach>
                </tbody>
                <tfoot>
                </tfoot>
              </table>
            </c:forEach>
          </div>

          <div class="well">
            <h4>
              <a href="<c:url value="/report/databaseKitsReport/download"/>?start=${dateFormatter.format(startDate)}&end=${dateFormatter.format(endDate)}"><i class="fa fa-arrow-circle-o-down font18" title="download"></i></a>&nbsp;
              <b>Profiles Entered into DNA Database</b>
              <span class="pull-right"><small>Date profile entered into DNA Database is between ${dateFormatter.format(startDate)} and ${dateFormatter.format(endDate)}</small></span>
            </h4>
            <c:forEach var="group" items="${databaseKitsReport.groups}">
              <table class="table">
                <tr class="groupTotal" data-clickable="enter-${group.groupId}">
                  <td style="width: 5%;"><span id="enter-${group.groupId}" class="glyphicon glyphicon-menu-down" aria-hidden="true" data-toggle="tooltip" data-placement="top" title="Details"></span></td>
                  <td style="width: 45%;"><b><c:out value="${group.agencyName}"/></b></td>
                  <td style="width: 50%;"><b>${group.rows.size()}</b> kits</td>
                </tr>
              </table>
              <table class="table table-striped kitRows hidden enter-${group.groupId}">
                <thead>
                  <tr>
                    <th style="width: 5%;"></th>
                    <th style="width: 45%;">Serial Number</th>
                    <th style="width: 50%;">DNA Database Entry Date</th>
                  </tr>
                </thead>
                <tbody>
                    <c:forEach var="row" items="${group.rows}">
                      <tr>
                        <td style="width: 5%;"></td>
                        <td style="width: 45%;"><a href="<c:url value="/admin/view?id=${row.id}"/>">${row.serialNumber}</a></td>
                        <td style="width: 50%;">${dateFormatter.format(row.dnaDatabaseEntryDate)}</td>
                      </tr>
                    </c:forEach>
                </tbody>
                <tfoot>
                </tfoot>
              </table>
            </c:forEach>
          </div>
            
            
          <div class="well">
            <h4>
              <a href="<c:url value="/report/hitsInKitsReport/download"/>?start=${dateFormatter.format(startDate)}&end=${dateFormatter.format(endDate)}"><i class="fa fa-arrow-circle-o-down font18" title="download"></i></a>&nbsp;
              <b>DNA Database Hits</b><span class="pull-right"><small>Date DNA Database hit received is between ${dateFormatter.format(startDate)} and ${dateFormatter.format(endDate)}</small></span>
            </h4>
            <c:forEach var="group" items="${hitsInKitsReport.groups}">
              <table class="table">
                <tr class="groupTotal" data-clickable="hit-${group.groupId}">
                  <td style="width: 5%;"><span id="hit-${group.groupId}" class="glyphicon glyphicon-menu-down" aria-hidden="true" data-toggle="tooltip" data-placement="top" title="Details"></span></td>
                  <td style="width: 45%;"><b><c:out value="${group.agencyName}"/></b></td>
                  <td style="width: 50%;"><b>${group.rows.size()}</b> kits</td>
                </tr>
              </table>
              <table class="table table-striped kitRows hidden hit-${group.groupId}">
                <thead>
                  <tr>
                    <th style="width: 5%;"></th>
                    <th style="width: 45%;">Serial Number</th>
                    <th style="width: 25%;">DNA Database Entry Date</th>
                    <th style="width: 25%;">Hit Date</th>
                  </tr>
                </thead>
                <tbody>
                    <c:forEach var="row" items="${group.rows}">
                      <tr>
                        <td style="width: 5%;"></td>
                        <td style="width: 45%;"><a href="<c:url value="/admin/view?id=${row.id}"/>">${row.serialNumber}</a></td>
                        <td style="width: 25%;">${dateFormatter.format(row.dnaDatabaseEntryDate)}</td>
                        <td style="width: 25%;">${dateFormatter.format(row.dnaDatabaseHitDate)}</td>
                      </tr>
                    </c:forEach>
                </tbody>
                <tfoot>
                </tfoot>
              </table>
            </c:forEach>
          </div>

            
        </div>
      </div>      
    </c:if>
        
  </jsp:attribute>
  <jsp:attribute name="scripts">
    <script src="<c:url value="../assets/js/saktrack-search.js"/>"></script>
    <script src="<c:url value="../assets/js/report-scripts.js"/>"></script>
    
    
    
    
  </jsp:attribute>
</t:page>