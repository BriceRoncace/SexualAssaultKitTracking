<%@page contentType="text/html" pageEncoding="UTF-8"%>
<%@ taglib prefix="t" tagdir="/WEB-INF/tags" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>

<t:page>
  <jsp:attribute name="head">
    <link href="<c:url value="/assets/css/pagination.css"/>" rel="stylesheet" type="text/css" />
  </jsp:attribute>
  <jsp:attribute name="body">
    <%@include file="../../includes/org-nav.jspf" %>
    <%@include file="../../includes/messages.jspf" %>
    <t:breadcrumb crumbActive="Search"/>
    <c:set var="searchRun" value="${page != null}"/>
    <h2 class="line-under"><c:out value="${userOrg.name}"/> - Sexual Assault Kit Search <span class="small"><small data-toggle-trigger class="clickable btn-link"><span data-toggleable class="${searchRun ? '' : 'hidden'}">show search</span><span data-toggleable class="${searchRun ? 'hidden' : ''}">hide search</span></small></span></h2>
    <form id="search-form" data-toggleable class="${searchRun ? 'hidden' : ''}" method="get">
      <div class="row">
        <div class="col-xs-2">
          <div class="form-group">
            <label class="control-label">Serial Number</label>
            <input type="text" data-focus class="form-control" name="serialNumber" value="${criteria.serialNumber}"/>
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
            <span class="clickable label thin label-primary" data-search-type="ON">On</span>
            <span class="clickable label thin label-default clickable" data-search-type="BEFORE">Before</span>
            <span class="clickable label thin label-default" data-search-type="AFTER">After</span>
            <span class="clickable label thin label-default" data-search-type="BETWEEN">Between</span>
            <input type="hidden" name="eventDate.searchType" value="${criteria.eventDate.searchType == null ? 'ON' : criteria.eventDate.searchType}"/>
            <div class="input-group">
              <input type="text" class="form-control hasDatePicker" name="eventDate.date1" value="${dateFormatter.format(criteria.eventDate.date1)}">
              <span class="input-group-addon invisible">-</span>
              <input type="text" class="form-control hasDatePicker invisible" name="eventDate.date2" value="${dateFormatter.format(criteria.eventDate.date2)}">
            </div>
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
            <span class="clickable label thin label-primary" data-search-type="ON">On</span>
            <span class="clickable label thin label-default clickable" data-search-type="BEFORE">Before</span>
            <span class="clickable label thin label-default" data-search-type="AFTER">After</span>
            <span class="clickable label thin label-default" data-search-type="BETWEEN">Between</span>
            <input type="hidden" name="collected.searchType" value="${criteria.collectedDate.searchType == null ? 'ON' : criteria.collectedDate.searchType}"/>
            <div class="input-group">
              <input type="text" class="form-control hasDatePicker" name="collectedDate.date1" value="${dateFormatter.format(criteria.collectedDate.date1)}">
              <span class="input-group-addon invisible">-</span>
              <input type="text" class="form-control hasDatePicker invisible" name="collectedDate.date2" value="${dateFormatter.format(criteria.collectedDate.date2)}">
            </div>
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
            <span class="clickable label thin label-primary" data-search-type="ON">On</span>
            <span class="clickable label thin label-default clickable" data-search-type="BEFORE">Before</span>
            <span class="clickable label thin label-default" data-search-type="AFTER">After</span>
            <span class="clickable label thin label-default" data-search-type="BETWEEN">Between</span>
            <input type="hidden" name="crimeDate.searchType" value="${criteria.crimeDate.searchType == null ? 'ON' : criteria.crimeDate.searchType}"/>
            <div class="input-group">
              <input type="text" class="form-control hasDatePicker" name="crimeDate.date1" value="${dateFormatter.format(criteria.crimeDate.date1)}">
              <span class="input-group-addon invisible">-</span>
              <input type="text" class="form-control hasDatePicker invisible" name="crimeDate.date2" value="${dateFormatter.format(criteria.crimeDate.date2)}">
            </div>
          </div>
        </div>
      </div>     
      
      <div class="row grey-background">
        <div class="col-xs-2">
          <div class="form-group">
            <label class="control-label">Meets Submission Criteria</label><br/>
            <div class="btn-group btn-group-xs" role="group">
              <button class="btn btn-default" type="button" data-btn-val="true">Yes</button>
              <button class="btn btn-default" type="button" data-btn-val="false">No</button>
              <input type="hidden" name="meetsSubmissionCriteria" value="${criteria.meetsSubmissionCriteria}">
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
            <div class="btn-group btn-group-xs" role="group">
              <button class="btn btn-default" type="button" data-btn-val="true">Yes</button>
              <button class="btn btn-default" type="button" data-btn-val="false">No</button>
              <input type="hidden" name="prosecutorAgrees" value="${criteria.prosecutorAgrees}">
            </div>
          </div>
        </div>
            
        <div class="col-xs-4">
          <div class="form-group">
            <label class="control-label">Planned Destruction Date</label>
            <span class="clickable label thin label-primary" data-search-type="ON">On</span>
            <span class="clickable label thin label-default clickable" data-search-type="BEFORE">Before</span>
            <span class="clickable label thin label-default" data-search-type="AFTER">After</span>
            <span class="clickable label thin label-default" data-search-type="BETWEEN">Between</span>
            <input type="hidden" name="plannedDestructionDate.searchType" value="${criteria.plannedDestructionDate.searchType == null ? 'ON' : criteria.plannedDestructionDate.searchType}"/>
            <div class="input-group">
              <input type="text" class="form-control hasDatePicker" name="plannedDestructionDate.date1" value="${dateFormatter.format(criteria.plannedDestructionDate.date1)}">
              <span class="input-group-addon invisible">-</span>
              <input type="text" class="form-control hasDatePicker invisible" name="plannedDestructionDate.date2" value="${dateFormatter.format(criteria.plannedDestructionDate.date2)}">
            </div>
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
            <span class="clickable label thin label-primary" data-search-type="ON">On</span>
            <span class="clickable label thin label-default clickable" data-search-type="BEFORE">Before</span>
            <span class="clickable label thin label-default" data-search-type="AFTER">After</span>
            <span class="clickable label thin label-default" data-search-type="BETWEEN">Between</span>
            <input type="hidden" name="completedDate.searchType" value="${criteria.completedDate.searchType == null ? 'ON' : criteria.completedDate.searchType}"/>
            <div class="input-group">
              <input type="text" class="form-control hasDatePicker" name="completedDate.date1" value="${dateFormatter.format(criteria.completedDate.date1)}">
              <span class="input-group-addon invisible">-</span>
              <input type="text" class="form-control hasDatePicker invisible" name="completedDate.date2" value="${dateFormatter.format(criteria.completedDate.date2)}">
            </div>
          </div>
        </div>
      </div>           

      <div class="row">
        <div class="col-xs-2">
          <div class="form-group">
            <label class="control-label">DNA Database Entry</label><br/>
            <div class="btn-group btn-group-xs" role="group">
              <button class="btn btn-default" type="button" data-btn-val="YES">Yes</button>
              <button class="btn btn-default" type="button" data-btn-val="NO">No</button>
              <button class="btn btn-default" type="button" data-btn-val="NA">NA</button>
              <input type="hidden" name="dnaDatabaseEntry" value="${criteria.dnaDatabaseEntry}">
            </div>
          </div>  
        </div>
        <div class="col-xs-4">
          <div class="form-group">
            <label class="control-label">DNA Database Hit Date</label>
            <span class="clickable label thin label-primary" data-search-type="ON">On</span>
            <span class="clickable label thin label-default clickable" data-search-type="BEFORE">Before</span>
            <span class="clickable label thin label-default" data-search-type="AFTER">After</span>
            <span class="clickable label thin label-default" data-search-type="BETWEEN">Between</span>
            <input type="hidden" name="dnaDatabaseHitDate.searchType" value="${criteria.dnaDatabaseHitDate.searchType == null ? 'ON' : criteria.dnaDatabaseHitDate.searchType}"/>
            <div class="input-group">
              <input type="text" class="form-control hasDatePicker" name="dnaDatabaseHitDate.date1" value="${dateFormatter.format(criteria.dnaDatabaseHitDate.date1)}">
              <span class="input-group-addon invisible">-</span>
              <input type="text" class="form-control hasDatePicker invisible" name="dnaDatabaseHitDate.date2" value="${dateFormatter.format(criteria.dnaDatabaseHitDate.date2)}">
            </div>
          </div>
        </div>
        <div class="col-xs-4">
          <div class="form-group">
            <label class="control-label">Expunged Date</label>
            <span class="clickable label thin label-primary" data-search-type="ON">On</span>
            <span class="clickable label thin label-default clickable" data-search-type="BEFORE">Before</span>
            <span class="clickable label thin label-default" data-search-type="AFTER">After</span>
            <span class="clickable label thin label-default" data-search-type="BETWEEN">Between</span>
            <input type="hidden" name="expungedDate.searchType" value="${criteria.expungedDate.searchType == null ? 'ON' : criteria.expungedDate.searchType}"/>
            <div class="input-group">
              <input type="text" class="form-control hasDatePicker" name="expungedDate.date1" value="${dateFormatter.format(criteria.expungedDate.date1)}">
              <span class="input-group-addon invisible">-</span>
              <input type="text" class="form-control hasDatePicker invisible" name="expungedDate.date2" value="${dateFormatter.format(criteria.expungedDate.date2)}">
            </div>
          </div>
        </div>
      </div>      
            
      <div class="row">
        <div class="col-xs-12">
          <div class="form-group pull-right">
            <button data-action="<c:url value="/search/download"/>" type="submit" class="btn btn-default"><i class="fa fa-arrow-circle-o-down font18" title="download"></i> Export</button>
            <button data-action="<c:url value="/search"/>" type="submit" class="btn btn-primary">Search</button>
          </div>
        </div>
      </div>
    </form>
    <hr/>
    <%@include file="../../includes/search-results.jspf" %>
  </jsp:attribute>
        
  <jsp:attribute name="scripts">
    <script type="text/javascript">
      <%@include file="../../../assets/js/saktrack-search.js" %>
    </script>
  </jsp:attribute>
</t:page>