<%@page contentType="text/html" pageEncoding="UTF-8"%>
<%@ taglib prefix="t" tagdir="/WEB-INF/tags" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>

<t:page>
  <jsp:attribute name="body">
    <%@include file="../../includes/org-nav.jspf" %>
    <%@include file="../../includes/messages.jspf" %>
    <t:breadcrumb crumbActive="Kit #${kit.serialNumber}"/>
    <h2 class="line-under">Sexual Assault Kit #${kit.serialNumber}
      <div class="pull-right">
        <a class="btn btn-xs btn-default" href="<c:url value="/timeline"/>?serialNumber=${kit.serialNumber}"><i class="fa fa-clock-o font14" aria-hidden="true"></i> Kit Timeline</a>   
        <c:choose>
          <c:when test="${kit.status == 'UNUSED'}">
            <button type="button" class="btn btn-xs btn-default" data-toggle="modal" data-target="#sendKitToMedLeModal"><span class="glyphicon glyphicon-share-alt"></span> Send Kit...</button>
          </c:when>
          <c:otherwise>
            <button type="button" class="btn btn-xs btn-default" data-toggle="modal" data-target="#sendKitModal" <c:if test="${disableSendKitButton}">disabled</c:if>><span class="glyphicon glyphicon-share-alt"></span> Send Kit...</button>
          </c:otherwise>
        </c:choose>
        <button type="button" class="btn btn-xs btn-danger" data-toggle="modal" data-target="#destroyKitModal" <c:if test="${!kit.destroyable}">disabled</c:if>><span class="glyphicon glyphicon-fire"></span> Destroy Kit...</button>
      </div>
    </h2>
      
    <form action="<c:url value='/law-enforcement/saveDetails'/>" method="POST">
      <input type="hidden" name="${_csrf.parameterName}" value="${_csrf.token}"/>
      <input type="hidden" name="id" value="${kit.id}"/>

      <%@include file="../../includes/law-enforcement-details.jspf" %>
      <c:if test="${kit.legalDetails.releasedForReview != null}">
        <%@include file="../../includes/legal-details-readonly.jspf" %>
      </c:if>
      
      <div class="row">
        <div class="col-sm-8">
          <b>Status:</b> ${kit.status.label}
        </div>
        <div class="col-sm-4">
          <div class="top10 pull-right">
            <c:if test="${kit.status == 'REQUIRES_RELEASE_FOR_LEGAL_REVIEW'}">
              <button data-action="<c:url value='/law-enforcement/releaseKitForReview'/>" type="submit" class="btn btn-primary">Release for Review</button>
            </c:if>
            <button data-action="<c:url value='/law-enforcement/saveDetails'/>" type="submit" class="btn btn-primary">Save</button>
          </div>
        </div>
      </div>
    </form>
      
    <div class="well-sm light-grey-background top20">
      <p><small><span class="glyphicon glyphicon-share-alt red-text"></span> = Required to send the kit to a lab (or before prosecutorial review).</small></p>
      <p><small><span class="glyphicon glyphicon-fire"></span>&nbsp;&nbsp;Before a kit may be destroyed it must either be analyzed at a lab or deemed to not meet submission requirements and be on or after the planned destruction date.</small></p>  
    </div>
        
    <%@include file="includes/modals/send-kit-to-med-le-modal.jspf" %>
    <%@include file="includes/modals/send-kit-modal.jspf" %>
    <%@include file="includes/modals/destroy-kit-modal.jspf" %>
    
  </jsp:attribute>
  <jsp:attribute name="scripts">
    <script src="<c:url value="/assets/js/saktrack.js"/>"></script>
    <script type="text/javascript">
      $(function() {
        $('[data-toggle="tooltip"]').tooltip();
        
        $(".modal").on('shown.bs.modal', function() {
          sakTrack.initOrganizationSelects($(this));
        });
      });
    </script>
  </jsp:attribute>
</t:page>