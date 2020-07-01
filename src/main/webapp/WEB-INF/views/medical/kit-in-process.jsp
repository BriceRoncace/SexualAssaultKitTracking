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
        <button type="button" class="btn btn-default btn-xs" data-toggle="modal" data-target="#sendKitModal"><span class="glyphicon glyphicon-share-alt"></span> Send Kit...</button>
        <button type="button" class="btn btn-default btn-xs" data-toggle="modal" data-target="#repurposeKitModal"><span class="glyphicon glyphicon-transfer"></span> Repurpose Kit...</button>
      </div>
    </h2>
    
    <form id="medDetailsForm" action="<c:url value='/medical/saveDetails'/>" method="POST">
      <input type="hidden" name="${_csrf.parameterName}" value="${_csrf.token}"/>
      <input type="hidden" name="id" value="${kit.id}"/>
      
      <%@include file="../../includes/expiration-date.jspf" %>
      <%@include file="../../includes/medical-details.jspf" %>
      
      <div class="row">
        <div class="col-sm-12">
          <div class="top10 pull-right">
            <button type="submit" class="btn btn-primary">Save</button>
          </div>
        </div>
      </div>
    </form>
    <%@include file="includes/modals/send-kit-modal.jspf" %>
    <%@include file="includes/modals/repurpose-kit-modal.jspf" %>
  </jsp:attribute>
  <jsp:attribute name="scripts">
    <script src="<c:url value="/assets/js/saktrack.js"/>"></script>
    <script type="text/javascript">
      $(function() {
        $("#sendKitModal").on('shown.bs.modal', function() {
          copyMedicalDetailsToSendForm();
          
          function copyMedicalDetailsToSendForm() {
            var $sendForm = $("#medSendForm");
            var $inputs = $("#medDetailsForm").find("input[type='text'], input[type='radio']:checked, select");
            $inputs.each(function(i, input) {
              var $input = $(input);
              $sendForm.append($("<input />").attr("type", "hidden").attr("name", $input.attr("name")).attr("value", $input.val()));
            });
          }
        });
        
        $(".modal").on('shown.bs.modal', function() {
          sakTrack.initOrganizationSelects($(this));
        });
        
      });
    </script>
  </jsp:attribute>
</t:page>
