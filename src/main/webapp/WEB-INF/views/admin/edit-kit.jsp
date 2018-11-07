<%@page contentType="text/html" pageEncoding="UTF-8"%>
<%@ taglib prefix="t" tagdir="/WEB-INF/tags" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>

<t:page>
  <jsp:attribute name="body">
    <%@include file="includes/admin-nav.jspf" %>
    <%@include file="../../includes/messages.jspf" %>
    <t:breadcrumb crumbOrganizationDashboard="${kit.currentAssignment}" crumbActive="Kit #${kit.serialNumber}"/>
    <h2 class="line-under">Sexual Assault Kit #${kit.serialNumber}
      <%@include file="includes/kit-actions-btn-dropdown.jspf" %>
    </h2>
    
    <form class="bottom30" action="<c:url value='/admin/saveDetails'/>" method="POST">
      <input type="hidden" name="${_csrf.parameterName}" value="${_csrf.token}"/>
      <input type="hidden" name="id" value="${kit.id}"/>
      
      <div class="row top15">
        <div class="form-group col-sm-3">
          <label class="control-label ">Expiration Date</label>
          <input type="text" class="form-control hasDatePicker" name="expirationDate" value="${dateFormatter.format(kit.expirationDate)}" />
        </div>
      </div>
      
      <div class="row top15">
        <div class="col-sm-12">
          <h4 class="line-under">Medical Data</h4>
        </div>
      </div>
      <%@include file="../../includes/medical-details.jspf" %>
      <div class="row top15">
        <div class="col-sm-12">
          <h4 class="line-under">Law Enforcement Data</h4>
        </div>
      </div>
      <%@include file="../../includes/law-enforcement-details.jspf" %>
      <div class="row top15">
        <div class="col-sm-12">
          <h4 class="line-under">Prosecuting Attorney Review</h4>
        </div>
      </div>
      <div class="row">
        <div class="form-group col-sm-12">
          <label class="control-label">Non-Submission Reason</label>
          <t:select from="${nonSubmissionReasons}" name="legalDetails.nonSubmissionReason" optionValue="label" value="${kit.legalDetails.nonSubmissionReason}" emptyOption="" cssClass="form-control"/>
        </div>
      </div>
      <div class="row">
        <div class="col-sm-3">
          <div class="form-group">
            <label class="control-label ">In Agreement with Agency Finding</label>
            <div class="radio radio-primary">
              <label class="radio-inline">
                <input type="radio" name="legalDetails.prosecutorAgrees" value="true" ${kit.legalDetails.prosecutorAgrees == true ? 'checked' : ''}/> Yes
              </label>
              <label class="radio-inline">
                <input type="radio" name="legalDetails.prosecutorAgrees" value="false" ${kit.legalDetails.prosecutorAgrees == false ? 'checked' : ''}/> No
              </label>
            </div>
          </div>
          <div class="form-group top20">
            <label class="control-label required">Reviewing Prosecutor</label>
            <input type="text" class="form-control" name="legalDetails.reviewingProsecutor" value="${kit.legalDetails.reviewingProsecutor}" />
          </div>
        </div>
        <div class="form-group col-sm-9">
          <label class="control-label required">Notes</label>
          <textarea rows="4" name="legalDetails.prosecutorNotes" class="form-control">${kit.legalDetails.prosecutorNotes}</textarea>
        </div>
      </div>
      <div class="row">
        <div class="col-sm-3">
          <div class="form-group">
            <label>Released for Review On</label>
            <input type="text" class="form-control hasDatePicker" name="legalDetails.releasedForReview" value="${dateFormatter.format(kit.legalDetails.releasedForReview)}"/>
          </div>
        </div>
        <div class="col-sm-3">
          <div class="form-group">
            <label>Review Finalized On</label>
            <input type="text" class="form-control hasDatePicker" name="legalDetails.reviewFinalized" value="${dateFormatter.format(kit.legalDetails.reviewFinalized)}"/>
          </div>
        </div>
      </div>
      <div class="row top15">
        <div class="col-sm-12">
          <h4 class="line-under">Lab Data</h4>
        </div>
      </div>
      <%@include file="../../includes/lab-details.jspf" %>
      
      <div id="reasonModal" class="modal fade" tabindex="-1" role="dialog">
        <div class="modal-dialog" role="document">
          <div class="modal-content">
            <div class="modal-header">
              <button type="button" class="close" data-dismiss="modal" aria-label="Close"><span aria-hidden="true">&times;</span></button>
              <h4 class="modal-title">Modification Explanation</h4>
            </div>
            <div class="modal-body">
              <p>Please provide a reason or explanation for this modification.  These notes will be visible in the admin change log.</p>
              <div class="row">
                <div class="col-sm-12">
                  <label class="required">Reason for Modification</label>
                  <textarea data-required name="reason" class="form-control" data-modal-focus></textarea>
                </div>
              </div>
            </div>
            <div class="modal-footer">
              <button type="button" class="btn btn-default" data-dismiss="modal">Cancel</button>
              <button type="submit" class="btn btn-primary">Save</button>
            </div>
          </div>
        </div>
      </div>
      
      <div class="row">
        <div class="col-sm-12">
          <div class="top10 pull-right">
            <button id="save-btn" type="button" class="btn btn-primary">Save</button>
          </div>
        </div>
      </div>
    </form>
  </jsp:attribute>
  <jsp:attribute name="scripts">
    <script type="text/javascript">
      $(function() {
        $('[data-toggle="tooltip"]').tooltip();
        $("#save-btn").click(function() { $("#reasonModal").modal(); });
      });
    </script>
  </jsp:attribute>
</t:page>
