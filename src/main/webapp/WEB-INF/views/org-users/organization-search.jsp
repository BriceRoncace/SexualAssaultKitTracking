<%@page contentType="text/html" pageEncoding="UTF-8"%>
<%@ taglib prefix="t" tagdir="/WEB-INF/tags" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>

<t:page>
  <jsp:attribute name="body">
    <%@include file="../../includes/org-nav.jspf" %>
    <%@include file="../../includes/messages.jspf" %>
    <t:breadcrumb crumbActive="Search"/>
    <c:set var="searchRun" value="${page != null}"/>
    <h2 class="line-under"><c:out value="${userOrg.name}"/> - Sexual Assault Kit Search <span class="small"><small onclick="$('[data-toggleable]').toggleClass('hidden');" class="clickable btn-link"><span data-toggleable class="${searchRun ? '' : 'hidden'}">show search</span><span data-toggleable class="${searchRun ? 'hidden' : ''}">hide search</span></small></span></h2>
    <form id="search-form" data-toggleable class="${searchRun ? 'hidden' : ''}" method="get">
      <div class="row">
        <div class="col-xs-6 col-md-2">
          <div class="form-group">
            <label class="control-label">Serial Number</label>
            <input type="text" data-focus class="form-control" name="serialNumber" value="${criteria.serialNumber}"/>
          </div>
        </div>
        <div class="col-xs-6 col-md-10">
          <div class="form-group pull-right">            
            <button data-action="<c:url value="/search"/>" type="submit" class="btn btn-primary">Search</button>
            <button data-action="<c:url value="/search/download"/>" type="submit" class="btn btn-default"><i class="fa fa-arrow-circle-o-down font18" title="download"></i> Export</button>
          </div>
        </div>
      </div>

      <div class="row grey-background">
        <div class="col-xs-12">
          <h4 class="line-under-white">Kit Timeline Details</h4>
        </div>
      </div>
          
      <div class="row grey-background">
        <div class="col-xs-2">
          <div class="form-group">
            <label class="control-label">Event Type</label>
            <t:select name="eventType" from="${eventTypes}" emptyOption="" cssClass="form-control" optionValue="label" value="${criteria.eventType}"/>
          </div>
        </div>
        <div class="col-xs-3">
          <div class="form-group">
            <label class="control-label">Actor Name</label>
            <input type="text" class="form-control" name="eventActor" value="${criteria.eventActor}"/>
          </div>
        </div>
          
        <c:choose>
          <c:when test="${userOrg.type == 'PROSECUTOR'}">
            <div class="col-xs-3">
              <div class="form-group">
                <label class="control-label">Organization</label>
                <t:select name="eventOrganization" from="${organizations}" emptyOption="" cssClass="form-control" optionKey="id" optionValue="name" value="${criteria.eventOrganization}"/>
              </div>
            </div>
          </c:when>
          <c:otherwise>
            <div class="col-xs-3">
              <div class="form-group">
                <label class="control-label">Organization</label>
                <p class="input-like-text"><c:out value="${userOrg.name}"/></p>
              </div>
            </div>
          </c:otherwise>
        </c:choose>
          
        <div class="col-xs-4">
          <div class="form-group">
            <label class="control-label">Event Date</label>
            <t:criteria-date name="eventDate" value="${criteria.eventDate}" showNone="true"/>
          </div>
        </div>  
      </div>
      
      <div class="row">
        <div class="col-xs-12">
          <h4 class="line-under">Medical Facility Details</h4>
        </div>
      </div> 
          
      <div class="row">
        <div class="col-xs-2">
          <div class="form-group">
            <label class="control-label">Victim Type</label>
            <t:select name="victimType" from="${victimTypes}" emptyOption="" cssClass="form-control" optionValue="label" value="${criteria.victimType}"/>
          </div>
        </div>
        <div class="col-xs-4">
          <div class="form-group">
            <label class="control-label">Collected Date</label>
            <t:criteria-date name="collectedDate" value="${criteria.collectedDate}" showNone="true"/>
          </div>
        </div>  
        <div class="col-xs-4">
          <div class="form-group">
            <label class="control-label">Requesting Agency</label>
            <t:select name="requestingLeAgencyId" from="${leOrganizations}" emptyOption="" cssClass="form-control" optionKey="id" optionValue="name" value="${criteria.requestingLeAgencyId}"/>
          </div>
        </div>
        <c:if test="${userOrg.type == 'PROSECUTOR'}">
          <div class="col-xs-2">
            <div class="form-group">
              <label class="control-label">Req. Agency Jurisdiction</label>
              <p class="input-like-text">${userOrg.jurisdiction.displayName}</p>
            </div>
          </div>
        </c:if>
      </div>
          
      <div class="row grey-background">
        <div class="col-xs-12">
          <h4 class="line-under-white">Law Enforcement Details</h4>
        </div>
      </div>     
      
      <div class="row grey-background">
        <div class="col-xs-2">
          <div class="form-group">
            <label class="control-label">LE Case Number</label>
            <input type="text" class="form-control" name="leCaseNumber" value="${criteria.leCaseNumber}"/>
          </div>
        </div>
        <div class="col-xs-3">
          <div class="form-group">
            <label class="control-label">Investigator</label>
            <input type="text" class="form-control" name="investigator" value="${criteria.investigator}"/>
          </div>
        </div>  
        <div class="col-xs-3">
          <div class="form-group">
            <label class="control-label">Crime</label>
            <input type="text" class="form-control" name="crime" value="${criteria.crime}"/>
          </div>
        </div>    
        <div class="col-xs-4">
          <div class="form-group">
            <label class="control-label">Crime Date</label>
            <t:criteria-date name="crimeDate" value="${criteria.crimeDate}" showNone="true"/>
          </div>
        </div>
      </div>     
      
      <div class="row grey-background">
        <div class="col-xs-2">
          <div class="form-group">
            <label class="control-label">Meets Submission Criteria</label><br/>
            <div class="btn-group btn-group-xs" data-toggle="buttons">
              <label class="btn btn-custom ${empty criteria.meetsSubmissionCriteria ? 'active' : ''}">
                <input type="radio" name="meetsSubmissionCriteria" value="" ${empty criteria.meetsSubmissionCriteria ? 'checked' : ''}/> Any
              </label>
              <label class="btn btn-custom ${criteria.meetsSubmissionCriteria == 'true' ? 'active' : ''}">
                <input type="radio" name="meetsSubmissionCriteria" value="true" ${criteria.meetsSubmissionCriteria == 'true' ? 'checked' : ''}/> Yes
              </label>
              <label class="btn btn-custom ${criteria.meetsSubmissionCriteria == 'false' ? 'active' : ''}">
                <input type="radio" name="meetsSubmissionCriteria" value="false" ${criteria.meetsSubmissionCriteria == 'false' ? 'checked' : ''}/> No
              </label>
            </div>
          </div>
        </div>
            
        <div class="form-group col-xs-3">
          <label class="control-label">Non-Submission Reason</label>
          <t:select from="${nonSubmissionReasons}" name="nonSubmissionReason" optionValue="label" value="${criteria.nonSubmissionReason}" emptyOption="" cssClass="form-control"/>
        </div>
            
        <div id="prosecutorAgrees" class="col-xs-3">
          <div class="form-group">
            <label class="control-label">Attorney Concurs with Non-Submission</label><br/>
            <div class="btn-group btn-group-xs" data-toggle="buttons">
              <label class="btn btn-custom ${empty criteria.prosecutorAgrees ? 'active' : ''}">
                <input type="radio" name="prosecutorAgrees" value="" ${empty criteria.prosecutorAgrees ? 'checked' : ''}/> Any
              </label>
              <label class="btn btn-custom ${criteria.prosecutorAgrees == 'true' ? 'active' : ''}">
                <input type="radio" name="prosecutorAgrees" value="true" ${criteria.prosecutorAgrees == 'true' ? 'checked' : ''}/> Yes
              </label>
              <label class="btn btn-custom ${criteria.prosecutorAgrees == 'false' ? 'active' : ''}">
                <input type="radio" name="prosecutorAgrees" value="false" ${criteria.prosecutorAgrees == 'false' ? 'checked' : ''}/> No
              </label>
            </div>
          </div>
        </div>
            
        <div class="col-xs-4">
          <div class="form-group">
            <label class="control-label">Planned Destruction Date</label>
            <t:criteria-date name="plannedDestructionDate" value="${criteria.plannedDestructionDate}" showNone="true"/>
          </div>
        </div>    
      </div>      
          
      <div class="row">
        <div class="col-xs-12">
          <h4 class="line-under">Laboratory Details</h4>
        </div>
      </div> 
          
      <div class="row">
        <div class="col-xs-2">
          <div class="form-group">
            <label class="control-label">Lab Case Number</label>
            <input type="text" class="form-control" name="labCaseNumber" value="${criteria.labCaseNumber}"/>
          </div>
        </div>
        <div class="col-xs-4">
          <div class="form-group">
            <label class="control-label">Completed Date</label>
            <t:criteria-date name="completedDate" value="${criteria.completedDate}" showNone="true"/>
          </div>
        </div>
      </div>           

      <div class="row">
        <div class="col-xs-2">
          <div class="form-group">
            <label class="control-label">DNA Database Entry</label><br/>
            <div class="btn-group btn-group-xs" data-toggle="buttons">
              <label class="btn btn-custom ${empty criteria.dnaDatabaseEntry ? 'active' : ''}">
                <input type="radio" name="dnaDatabaseEntry" value="" ${empty criteria.dnaDatabaseEntry ? 'checked' : ''}/> Any
              </label>
              <label class="btn btn-custom ${criteria.dnaDatabaseEntry == 'YES' ? 'active' : ''}">
                <input type="radio" name="dnaDatabaseEntry" value="YES" ${criteria.dnaDatabaseEntry == 'YES' ? 'checked' : ''}/> Yes
              </label>
              <label class="btn btn-custom ${criteria.dnaDatabaseEntry == 'NO' ? 'active' : ''}">
                <input type="radio" name="dnaDatabaseEntry" value="NO" ${criteria.dnaDatabaseEntry == 'NO' ? 'checked' : ''}/> No
              </label>
              <label class="btn btn-custom ${criteria.dnaDatabaseEntry == 'NA' ? 'active' : ''}">
                <input type="radio" name="dnaDatabaseEntry" value="NA" ${criteria.dnaDatabaseEntry == 'NA' ? 'checked' : ''}/> NA
              </label>
            </div>
          </div>  
        </div>
        <div class="col-xs-4">
          <div class="form-group">
            <label class="control-label">DNA Database Hit Date</label>
            <t:criteria-date name="dnaDatabaseHitDate" value="${criteria.dnaDatabaseHitDate}" showNone="true"/>
          </div>
        </div>
        <div class="col-xs-4">
          <div class="form-group">
            <label class="control-label">Expunged Date</label>
            <t:criteria-date name="expungedDate" value="${criteria.expungedDate}" showNone="true"/>
          </div>
        </div>
      </div>      
            
      <div class="row">
        <div class="col-xs-12">
          <div class="form-group pull-right">            
            <button data-action="<c:url value="/search"/>" type="submit" class="btn btn-primary">Search</button>
            <button data-action="<c:url value="/search/download"/>" type="submit" class="btn btn-default"><i class="fa fa-arrow-circle-o-down font18" title="download"></i> Export</button>
          </div>
        </div>
      </div>
      <hr/>
    </form>
    <%@include file="../../includes/search-results.jspf" %>
  </jsp:attribute>
  <jsp:attribute name="scripts">
  </jsp:attribute>
</t:page>