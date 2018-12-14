<%@page contentType="text/html" pageEncoding="UTF-8"%>
<%@ taglib prefix="t" tagdir="/WEB-INF/tags" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>

<t:page>
  <jsp:attribute name="body">
    <%@include file="../../includes/org-nav.jspf" %>
    <%@include file="../../includes/messages.jspf" %>
    <t:breadcrumb crumbActive="Kit #${kit.serialNumber}"/>        
    <h2 class="line-under">Sexual Assault Kit #${kit.serialNumber}</h2>
    
    <div class="row bottom10">
      <div class="col-sm-6">
        <label class="control-label">Agency <span class="glyphicon glyphicon-question-sign hover-pointer med-blue-text" data-toggle="tooltip" title="Law enforcement agency currently assigned to kit (usually this means they have physical custody of the kit, but it may be in transit to this agency)."></span></label>
        <div class="controls readonly">
          <c:out value="${kit.currentAssignment.name}"/>
        </div>
      </div>
      <div class="col-sm-6">
        <label class="control-label">Agency Requesting Kit <span class="glyphicon glyphicon-question-sign hover-pointer med-blue-text" data-toggle="tooltip" title="Law enforcement agency requesting the kit as documented by the medical organization collecting the evidence."></span></label>
        <div class="controls readonly">
          <c:out value="${kit.medicalDetails.requestingLeAgency}"/>
        </div>
      </div>
    </div>
    
    <div class="row">
      <div class="form-group col-sm-3">
        <label class="control-label required">Case Number</label>
        <input type="text" class="form-control read-only" name="leDetails.caseNumber" value="${kit.leDetails.caseNumber}" readonly/>
      </div>
      <div class="form-group col-sm-3">
        <label class="control-label required">Investigator</label>
        <input type="text" class="form-control" name="leDetails.investigator" value="${kit.leDetails.investigator}" readonly/>
      </div>
      <div class="form-group col-sm-3">
        <label class="control-label required">Crime</label>
        <input type="text" class="form-control" name="leDetails.crime" value="${kit.leDetails.crime}" readonly/>
      </div>
      <div class="form-group col-sm-3">
        <label class="control-label required">Crime Date</label>
        <input type="text" class="form-control" name="leDetails.crimeDate" value="${dateFormatter.format(kit.leDetails.crimeDate)}" readonly/>
      </div>
    </div>
    <div class="row">
      <div class="form-group col-sm-3">
        <label class="control-label ">FBI Lab</label>
        <input type="text" class="form-control" name="leDetails.outsourcedLabName" value="${kit.leDetails.outsourcedLabName}" readonly/>
      </div>
      <div class="form-group col-sm-3">
        <label class="control-label required">Planned Destruction Date</label>
        <input type="text" class="form-control" name="leDetails.plannedDestructionDate" value="${dateFormatter.format(kit.leDetails.plannedDestructionDate)}" readonly/>
      </div>
      <div class="form-group col-sm-3">
        <label class="control-label">Notify Victim Prior to Destruction <span class="glyphicon glyphicon-question-sign hover-pointer med-blue-text" data-toggle="tooltip" title="Law enforcement agency will check this box when the victim has requested notification from the law enforcement agency prior to kit destruction.  When checked, an automated email will be sent 6 months prior to the planned destruction date to all law enforcment organization administrators with the requesting agency reminding them to notify the victim concerning the upcoming planned destruction."></span></label>
        <div class="checkbox">
          <label><t:checkbox id="destructionNotification" name="leDetails.plannedDestructionNotificationRequested" disabled="true" checked="${kit.leDetails.plannedDestructionNotificationRequested}"/></label>
        </div>
      </div>
    </div>
    <div class="row">
      <div class="col-sm-3">
        <div class="form-group">
          <label class="control-label required">Meets Submission Requirements</label>
          <p>${kit.leDetails.meetsSubmissionCriteria == true ? 'Yes' : 'No'}</p>
        </div>
      </div>
      <div class="form-group col-sm-9">
        <label class="control-label required">Non-Submission Reason</label>
        <t:select from="${nonSubmissionReasons}" name="leDetails.nonSubmissionReason" optionValue="label" value="${kit.leDetails.nonSubmissionReason}" emptyOption="" cssClass="form-control" disabled="true"/>
      </div>
    </div>
        
    <form action="<c:url value='/legal/saveDetails'/>" method="POST">
      <input type="hidden" name="${_csrf.parameterName}" value="${_csrf.token}"/>
      <input type="hidden" name="id" value="${kit.id}"/>  
      <c:if test="${kit.leDetails.meetsSubmissionCriteria != null && kit.leDetails.meetsSubmissionCriteria == false}">
        <div class="row top15">
          <div class="col-sm-12">
            <h4 class="line-under">
              Prosecuting Attorney Review
              <span class="pull-right small">Released for Review on ${dateFormatter.format(kit.legalDetails.releasedForReview)}</span>
            </h4>
          </div>
        </div>

        <div class="row">
          <div class="col-sm-12">
            <label class="control-label ">Non-Submission Reason</label>
            <p>${kit.legalDetails.nonSubmissionReason.label}</p>
          </div>
        </div>    
            
        <div class="row">
          <div class="form-group col-sm-12">
            <label class="control-label required">Notes</label>
            <textarea rows="4" id="notes" name="legalDetails.prosecutorNotes" class="form-control">${kit.legalDetails.prosecutorNotes}</textarea>
          </div>
        </div>
        <div class="row">
          <div class="col-sm-12">
            <div class="top10 pull-right">
              <button type="submit" class="btn btn-primary">Save</button>
              <button type="button" class="btn btn-default" data-toggle="modal" data-target="#disagreeModal"><span class="glyphicon glyphicon-thumbs-down"></span> Disagree...</button>
              <button type="button" class="btn btn-default" data-toggle="modal" data-target="#agreeModal"><span class="glyphicon glyphicon-thumbs-up"></span> Agree...</button>
            </div>
          </div>
        </div>
      </c:if>
    </form>
    <%@include file="includes/modals/agree-decision-modal.jspf" %>
    <%@include file="includes/modals/disagree-decision-modal.jspf" %>
  </jsp:attribute>
  <jsp:attribute name="scripts">
    <script src="<c:url value="/assets/js/saktrack.js"/>"></script>
    <script type="text/javascript">
      $(function() {
        $('[data-toggle="tooltip"]').tooltip();
        
        $("#agreeModal, #disagreeModal").on('shown.bs.modal', function() {
          $("[name='notes'").val($("#notes").val());
        });
        
        $("#sendKitModal").on('shown.bs.modal', function() {
          $.getJSON( "<c:url value="/organizations?type=LAB"/>", function(orgs) {
            sakTrack.initOrganizationSelect(orgs, "#labOrganizationsSelect");
          });
          
          $.getJSON( "<c:url value="/organizations?type=LAW_ENFORCEMENT"/>", function(orgs) {
            sakTrack.initOrganizationSelect(orgs, "#lawEnforcementOrganizationsSelect");
          });
        });
      });
    </script>
  </jsp:attribute>
</t:page>