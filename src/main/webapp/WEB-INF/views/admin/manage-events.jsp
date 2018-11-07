<%@page contentType="text/html" pageEncoding="UTF-8"%>
<%@ taglib prefix="t" tagdir="/WEB-INF/tags" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>

<t:page>
  <jsp:attribute name="body">
    <%@include file="includes/admin-nav.jspf" %>
    <%@include file="../../includes/messages.jspf" %>
    <t:breadcrumb crumbOrganizationDashboard="${kit.currentAssignment}" crumbKit="${kit}" crumbActive="Manage Kit Timeline"/>
    <h2>
      Sexual Assault Kit #${kit.serialNumber}
      <c:if test="${kit.questionableEvents}">
        <a href="#questionable-modal" data-toggle="modal" title="Clear Questionable Events Flag"><img src="<c:url value="/assets/images/cancel-flag.gif"/>" height="25" width="25" alt="Clear Flag" /></a>
        </c:if>
        <%@include file="includes/kit-actions-btn-dropdown.jspf" %>
    </h2>

    <h3 class="topNeg10 bottom20 line-under"><small>Manage Kit Timeline</small></h3>
          
    <div class="row">
      <div class="col-sm-6">

        <div class="col-xs-12">
          Add event:
          <a class="btn btn-default btn-xs" href="<c:url value="/admin/enterEvent"/>?kitId=${kit.id}&eventType=SEND">Send</a>
          <a class="btn btn-default btn-xs" href="<c:url value="/admin/enterEvent"/>?kitId=${kit.id}&eventType=RECEIVE">Receive</a>
          <a class="btn btn-default btn-xs" href="<c:url value="/admin/enterEvent"/>?kitId=${kit.id}&eventType=DESTROY">Destroy</a>
          <a class="btn btn-default btn-xs" href="<c:url value="/admin/enterEvent"/>?kitId=${kit.id}&eventType=REPURPOSE">Repurpose</a>
        </div>

        <table class="table">
          <tr>
            <th colspan="6">Kit Timeline</th>
          </tr>
          <c:forEach var="coc" items="${kit.chainOfCustody}" varStatus="s">
            <tr class="${coc.grabbedOutOfOrder ? 'warning' : ''}">
              <td>
                <c:if test="${coc.eventFlag != null}">
                  <a title="${coc.eventFlag.label}"><span class="glyphicon glyphicon-flag dark-grey-text cursor-default-pointer"></span></a>
                </c:if> <c:out value="${coc.prettyPrint(true)}"/></td>
              <td>
                <c:if test="${coc.grabbedOutOfOrder}">
                  <button type="button" title="Clear Questionable Event" class="btn-link" data-reason-modal-action="<c:url value="/admin/clearGrab"/>" data-reason-modal-kit-id="${kit.id}" data-reason-modal-event-id="${coc.id}" data-reason-modal-submit-label="Clear" data-reason-modal-title="Clear Questionable Event Flag" data-reason-modal-verbiage="Please provide the reason why you are clearing this flag."><img src="<c:url value="/assets/images/cancel-flag.gif"/>" height="20" width="20" alt="Clear Flag" /></button>
                </c:if>
              </td>
              <td>
                <c:if test="${!s.first}">
                  <button type="button" title="Move Up" class="btn-link" data-reason-modal-action="<c:url value="/admin/moveEvent?direction=UP"/>" data-reason-modal-kit-id="${kit.id}" data-reason-modal-event-id="${coc.id}" data-reason-modal-submit-label="Reorder" data-reason-modal-title="Reorder Kit Timeline Event" data-reason-modal-verbiage="Please provide the reason why you are reordering this event (moving it <strong>up</strong>)."><span class="glyphicon glyphicon-chevron-up"></span></button>
                  </c:if>
              </td>
              <td>
                <c:if test="${!s.last}">
                  <button type="button" title="Move Down" class="btn-link" data-reason-modal-action="<c:url value="/admin/moveEvent?direction=DOWN"/>" data-reason-modal-kit-id="${kit.id}" data-reason-modal-event-id="${coc.id}" data-reason-modal-submit-label="Reorder" data-reason-modal-title="Reorder Kit Timeline Event" data-reason-modal-verbiage="Please provide the reason why you are reordering this event (moving it <strong>down</strong>.)"><span class="glyphicon glyphicon-chevron-down"></span></button>
                  </c:if>
              </td>
              <td>
                <a title="Edit" href="<c:url value="/admin/editEvent"/>?kitId=${kit.id}&eventId=${coc.id}"><span class="glyphicon glyphicon-pencil"></span></a>
              </td>
              <td>
                <button type="button" title="Delete" class="btn-link" data-reason-modal-action="<c:url value="/admin/removeEvent"/>" data-reason-modal-kit-id="${kit.id}" data-reason-modal-event-id="${coc.id}" data-reason-modal-submit-label="Delete" data-reason-modal-title="Delete Kit Timeline Event" data-reason-modal-verbiage="Please provide the reason why you are deleting this event below."><span class="glyphicon glyphicon-trash"></span></button>
              </td>
            </tr>
          </c:forEach>
        </table>

        <div id="reason-modal" class="modal fade" tabindex="-1" role="dialog">
          <div class="modal-dialog" role="document">
            <div class="modal-content">
              <div class="modal-header">
                <button type="button" class="close" data-dismiss="modal" aria-label="Close"><span aria-hidden="true">&times;</span></button>
                <h4 class="modal-title" id="reason-modal-title">Are you sure?</h4>
              </div>

              <form id="reason-modal-form" method="POST">
                <input type="hidden" name="${_csrf.parameterName}" value="${_csrf.token}"/>
                <input id="reason-modal-kit-id" type="hidden" name="kitId" />
                <input id="reason-modal-event-id" type="hidden" name="eventId"/>

                <div class="modal-body">
                  <p id="reason-modal-verbiage">Please provide a reason for this action below.</p>
                  <div class="row">
                    <div class="form-group col-xs-12 bottom0">
                      <label class="control-label required">Reason</label>
                      <textarea rows="1" data-required class="form-control" name="reason" data-modal-focus></textarea>
                    </div>
                  </div>  
                </div>
                <div class="modal-footer">
                  <button type="button" class="btn btn-default" data-dismiss="modal">Cancel</button>
                  <button id="reason-modal-submit-btn" type="submit" class="btn btn-primary">Submit</button>
                </div>
              </form>
            </div>
          </div>
        </div>

        <table class="table table-condensed">
          <tr>
            <th colspan="3">Known Missing Sent Events:</th>
          </tr>
          <c:forEach var="missingEvent" items="${missingSendEvents}">
            <tr>
              <td>From: <c:out value="${missingEvent.from.name}"/></td>
              <td>To: <c:out value="${missingEvent.to.name}"/></td>
              <td>
                <a class="btn btn-default btn-xs" href="<c:url value="/admin/enterEvent"/>?kitId=${kit.id}&eventType=SEND&actorOrgId=${missingEvent.from.id}&fromOrgId=${missingEvent.from.id}&toOrgId=${missingEvent.to.id}">Send</a>
              </td>
            </tr>
          </c:forEach>
        </table>
      </div>

      <div class="col-sm-6">
        <div class="bottom20">Status: <strong>${kit.status.label}</strong></div>
        <form action="<c:url value="/admin/setAssignment"/>" method="POST">
          <input type="hidden" name="${_csrf.parameterName}" value="${_csrf.token}"/>
          <input type="hidden" name="id" value="${kit.id}"/>
          <div class="panel panel-primary">
            <div class="panel-heading">Current Assignment</div>
            <div class="panel-body">
              <div class="row">
                <div class="form-group col-xs-12">
                  <select class="form-control" name="currentAssignmentId">
                    <option></option>
                    <c:forEach var="org" items="${organizations}">
                      <option value="${org.id}" <c:if test="${org.id == kit.currentAssignment.id}">selected</c:if>><c:out value="${org.name}"/></option>
                    </c:forEach>
                  </select>
                </div>
              </div>
              <div class="row">
                <div class="form-group col-xs-12 bottom0">
                  <label class="control-label required">Reason for Change</label>
                  <textarea rows="1" data-required class="form-control" name="reason"></textarea>
                </div>
              </div>  
            </div>
            <div class="panel-footer text-right">
              <button type="submit" class="btn btn-primary">Save</button>
            </div>
          </div>
        </form>
        <c:if test="${eventType != null}">
          <c:choose>
            <c:when test="${event.id != null}">
              <c:set var="actionUrl" value="/admin/updateEvent"/>
            </c:when>
            <c:otherwise>
              <c:set var="actionUrl" value="/admin/newEvent"/>
            </c:otherwise>
          </c:choose>
          <form action="<c:url value="${actionUrl}"/>" method="POST">
            <input type="hidden" name="${_csrf.parameterName}" value="${_csrf.token}"/>
            <input type="hidden" name="kitId" value="${kit.id}"/>
            <c:if test="${event.id != null}">
              <input type="hidden" name="eventId" value="${event.id}"/>
            </c:if>
            <input type="hidden" name="eventType" value="${eventType}"/>

            <div class="panel panel-primary">
              <div class="panel-heading">Event</div>
              <div class="panel-body">
                <h3 class="top0">The kit was ${eventType.pastTenseLabel}</h3>

                <div class="row">
                  <div class="form-group col-xs-4">
                    <label class="control-label required">On</label>
                    <input type="text" data-required class="form-control hasDatePicker" name="eventDate" value="${dateFormatter.format(event.eventDate)}" />
                  </div>
                  <div class="form-group col-xs-8">
                    <label class="control-label required">By This Person</label>
                    <input type="text" data-required class="form-control" name="actor" value="${event.actor}" />
                  </div>
                </div>

                <div class="row">
                  <div class="form-group col-xs-12">
                    <label class="control-label required">With This Organization</label>
                    <select data-required class="form-control" name="actorOrgId">
                      <option></option>
                      <c:forEach var="org" items="${organizations}">
                        <option value="${org.id}" <c:if test="${org.id == event.actorOrganization.id}">selected</c:if>><c:out value="${org.name}"/></option>
                      </c:forEach>
                    </select>
                  </div>
                </div>
                <c:choose>
                  <c:when test="${eventType == 'SEND'}">
                    <div class="row">
                      <div class="form-group col-xs-12">
                        <label class="control-label required">To</label>
                        <select data-required class="form-control" name="toOrgId">
                          <option></option>
                          <c:forEach var="org" items="${organizations}">
                            <option value="${org.id}" <c:if test="${org.id == event.to.id}">selected</c:if>><c:out value="${org.name}"/></option>
                          </c:forEach>
                        </select>
                      </div>
                    </div>
                    <div class="row">
                      <div class="form-group col-xs-12">
                        <label class="control-label">Report Milestone</label>
                        <select class="form-control" name="eventFlag">
                          <option></option>
                          <c:forEach var="eventFlag" items="${sendEventFlags}">
                            <option value="${eventFlag}" <c:if test="${eventFlag == event.eventFlag}">selected</c:if>>${eventFlag.label}</option>
                          </c:forEach>
                        </select>
                      </div>
                    </div>
                  </c:when>

                  <c:when test="${eventType == 'RECEIVE'}">
                    <div class="row">
                      <div class="form-group col-xs-12">
                        <label class="control-label required">From</label>
                        <select data-required class="form-control" name="fromOrgId">
                          <option></option>
                          <c:forEach var="org" items="${organizations}">
                            <option value="${org.id}" <c:if test="${org.id == event.from.id}">selected</c:if>><c:out value="${org.name}"/></option>
                          </c:forEach>
                        </select>
                      </div>
                    </div>
                    <div class="row">
                      <div class="form-group col-xs-12">
                        <label class="control-label">Report Milestone</label>
                        <select class="form-control" name="eventFlag">
                          <option></option>
                          <c:forEach var="eventFlag" items="${receiveEventFlags}">
                            <option value="${eventFlag}" <c:if test="${eventFlag == event.eventFlag}">selected</c:if>>${eventFlag.label}</option>
                          </c:forEach>
                        </select>
                      </div>
                    </div>
                  </c:when>
                </c:choose>

                <div class="row">
                  <div class="form-group col-xs-12">
                    <label class="control-label">Event Notes</label>
                    <textarea class="form-control" name="notes">${event.notes}</textarea>
                  </div>
                </div>

                <div class="row">
                  <div class="form-group col-xs-12 bottom0">
                    <label class="control-label required">Reason for Change</label>
                    <textarea rows="1" data-required class="form-control" name="reason"></textarea>
                  </div>
                </div>  

              </div>
              <div class="panel-footer text-right">
                <a href="<c:url value="/admin/manageEvents"/>?kitId=${kit.id}" class="btn btn-default">Cancel</a>
                <button type="submit" class="btn btn-primary">Save</button>
              </div>
            </div>
          </form>
        </c:if>
      </div>
    </div>

    <div id="questionable-modal" class="modal fade" tabindex="-1" role="dialog">
      <div class="modal-dialog" role="document">
        <div class="modal-content">
          <div class="modal-header">
            <button type="button" class="close" data-dismiss="modal" aria-label="Close"><span aria-hidden="true">&times;</span></button>
            <h4 class="modal-title">Clear Questionable Event Flag</h4>
          </div>
          <form action="<c:url value='/admin/clearQuestionable'/>" method="POST">
            <input type="hidden" name="${_csrf.parameterName}" value="${_csrf.token}"/>
            <div class="modal-body">
              <p>This kit has been flagged as containing timeline events that look questionable.  Usually this means that a receive event could not be paired with a corresponding send event from the expected organization.  Please ensure the kit's timeline events are valid before clearing this flag.</p>
              <p>This kit may be re-flagged in the future if its timeline is modified (either by an administrator or via normal kit processing) and questionable events are still deemed to exist.</p>
              <input type="hidden" name="kitId" value="${kit.id}"/>
              <div class="row">
                <div class="form-group col-xs-12 bottom0">
                  <label class="control-label required">Reason for Clearing Questionable Flag</label>
                  <textarea rows="1" data-required class="form-control" name="reason"></textarea>
                </div>
              </div>
            </div>
            <div class="modal-footer">
              <button type="button" class="btn btn-default" data-dismiss="modal">Close</button>
              <button type="submit" class="btn btn-primary">Save</button>
            </div>
          </form>
        </div>
      </div>
    </div>
  </jsp:attribute>
  <jsp:attribute name="scripts">
    <script type="text/javascript">
      $(function () {
        $("[data-reason-modal-action]").click(showReasonModal);

        function showReasonModal() {
          var $el = $(this);
          $("#reason-modal-form").attr("action", $el.data("reason-modal-action"));
          $("#reason-modal-kit-id").val($el.data("reason-modal-kit-id"));
          $("#reason-modal-event-id").val($el.data("reason-modal-event-id"));
          setHtmlIfDefined("#reason-modal-title", $el.data("reason-modal-title"));
          setHtmlIfDefined("#reason-modal-verbiage", $el.data("reason-modal-verbiage"));
          setHtmlIfDefined("#reason-modal-submit-btn", $el.data("reason-modal-submit-label"));
          $("#reason-modal").modal();
        }

        function setHtmlIfDefined(selector, html) {
          if (html) {
            $(selector).html(html);
          }
        }
      });
    </script>
  </jsp:attribute>
</t:page>