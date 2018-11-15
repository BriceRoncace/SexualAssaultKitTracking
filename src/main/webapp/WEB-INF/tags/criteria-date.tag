<%@tag trimDirectiveWhitespaces="true" %>
<%@taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@taglib prefix="fn" uri="http://java.sun.com/jsp/jstl/functions"%>

<%@attribute name="name" required="true" type="java.lang.String" %>
<%@attribute name="value" required="true" type="gov.idaho.isp.saktrack.domain.search.CriteriaDate" %>
<%@attribute name="showNoneOption" required="false" type="java.lang.Boolean" %>
<c:set var="d1Id" value="${fn:replace(fn:replace(fn:replace(name, ']', '-'), '[', '-'), '.', '-')}-1"/>
<c:set var="d2Id" value="${fn:replace(fn:replace(fn:replace(name, ']', '-'), '[', '-'), '.', '-')}-2"/>

<span class="left10">
  <div class="btn-group btn-group-toggle" data-toggle="buttons">
    <label class="btn btn-xxs btn-default ${value.searchType == 'ON' ? 'active' : ''}" onclick="$('#${d1Id}').attr('disabled', false); $('#${d2Id}').addClass('invisible').val('');$('#${d2Id}-span').addClass('invisible')">
      <input type="radio" name="${name}.searchType" value="ON" ${value.searchType == 'ON' ? 'checked' : ''}/> On
    </label>
    <label class="btn btn-xxs btn-default ${value.searchType == 'BEFORE' ? 'active' : ''}" onclick="$('#${d1Id}').attr('disabled', false); $('#${d2Id}').addClass('invisible').val('');$('#${d2Id}-span').addClass('invisible')">
      <input type="radio" name="${name}.searchType" value="BEFORE" ${value.searchType == 'BEFORE' ? 'checked' : ''}/> Before
    </label>
    <label class="btn btn-xxs btn-default ${value.searchType == 'AFTER' ? 'active' : ''}" onclick="$('#${d1Id}').attr('disabled', false); $('#${d2Id}').addClass('invisible').val('');$('#${d2Id}-span').addClass('invisible')">
      <input type="radio" name="${name}.searchType" value="AFTER" ${value.searchType == 'AFTER' ? 'checked' : ''}/> After
    </label>
    <label class="btn btn-xxs btn-default ${value.searchType == 'BETWEEN' ? 'active' : ''}" onclick="$('#${d1Id}').attr('disabled', false); $('#${d2Id}').removeClass('invisible').val('');$('#${d2Id}-span').removeClass('invisible')">
      <input type="radio" name="${name}.searchType" value="BETWEEN" ${value.searchType == 'BETWEEN' ? 'checked' : ''}/> Between
    </label>
    <c:if test="${showNoneOption}">
      <label class="btn btn-xxs btn-default ${value.searchType == 'NONE' ? 'active' : ''}" onclick="$('#${d1Id}').attr('disabled', true).val(''); $('#${d2Id}').addClass('invisible').val('');$('#${d2Id}-span').addClass('invisible')">
        <input type="radio" name="${name}.searchType" value="NONE" ${value.searchType == 'NONE' ? 'checked' : ''}/> None
      </label>
    </c:if>
  </div>
</span>
<div class="input-group">
  <input id="${d1Id}" type="text" class="form-control hasDatePicker" ${value.searchType == 'NONE' ? 'disabled' : ''} name="${name}.date1" value="${dateFormatter.format(value.date1)}">
  <span id="${d2Id}-span" class="input-group-addon ${value.searchType == 'BETWEEN' ? '' : 'invisible'}">-</span>
  <input id="${d2Id}" type="text" class="form-control hasDatePicker ${value.searchType == 'BETWEEN' ? '' : 'invisible'}" name="${name}.date2" value="${dateFormatter.format(value.date2)}">
</div>