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
        <button type="button" class="btn btn-xs btn-default" data-toggle="modal" data-send-kit="${kit.serialNumber}" data-kit-from="${kit.currentCustody.from.id}" data-target="#sendKitToLawModal"><span class="glyphicon glyphicon-share-alt"></span> Send Kit...</button>
        <button type="button" class="btn btn-default btn-xs" data-toggle="modal" data-target="#repurposeKitModal"><span class="glyphicon glyphicon-transfer"></span> Repurpose Kit...</button>
      </div>
    </h2>
    <form action="<c:url value='/lab/saveDetails'/>" method="POST">
      <input type="hidden" name="${_csrf.parameterName}" value="${_csrf.token}"/>
      <input type="hidden" name="id" value="${kit.id}"/>

      <div class="row">
        <div class="form-group col-sm-3">
          <label class="control-label ">Expiration Date</label><br/>
          ${dateFormatter.format(kit.expirationDate)}
        </div>
      </div>
      <%@include file="../../includes/lab-details.jspf" %>

      <div class="row">
        <div class="col-sm-12">
          <div class="top10 pull-right">
            <button type="submit" class="btn btn-primary">Save</button>
          </div>
        </div>
      </div>
    </form>
      
    <%@include file="includes/modals/repurpose-kit-modal.jspf" %>
  </jsp:attribute>
  <jsp:attribute name="scripts">
    <script src="<c:url value="/assets/js/saktrack.js"/>"></script>
    <script type="text/javascript">
      $(function() {
        
        $(".modal").on('shown.bs.modal', function() {
          $(this).find("select[data-organizations-url]").each(function(i, select){
            var $select = $(select);
            
            $.getJSON($select.data('organizations-url'), function(orgs) {
              sakTrack.initOrganizationSelect(orgs, $select);
            });
          });
        });
        
      });
    </script>
  </jsp:attribute>
</t:page>
    
<%@include file="includes/modals/send-used-kit-modal.jspf" %>
