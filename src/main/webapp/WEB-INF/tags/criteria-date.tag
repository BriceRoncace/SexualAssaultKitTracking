<%@tag trimDirectiveWhitespaces="true" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>

<%@attribute name="name" required="true" type="java.lang.String" %>
<%@attribute name="value" required="true" type="gov.idaho.isp.saktrack.domain.search.CriteriaDate" %>
<%@attribute name="showNone" required="false" type="java.lang.Boolean" %>

<span class="pull-right">
  <div class="btn-group" data-toggle="buttons">
    <label class="btn btn-custom btn-label ${empty value || value.searchType == 'ON' ? 'active' : ''}" onclick="$('[data-${name}-toggle]').addClass('hidden').val(''); $('[data-${name}-input]').attr('disabled', false);">
      <input type="radio" name="${name}.searchType" value="ON" ${empty value || value.searchType == 'ON' ? 'checked' : ''}/> On
    </label>
    <label class="btn btn-custom btn-label ${value.searchType == 'BEFORE' ? 'active' : ''}" onclick="$('[data-${name}-toggle]').addClass('hidden').val(''); $('[data-${name}-input]').attr('disabled', false);">
      <input type="radio" name="${name}.searchType" value="BEFORE" ${value.searchType == 'BEFORE' ? 'checked' : ''}/> Before
    </label>
    <label class="btn btn-custom btn-label ${value.searchType == 'AFTER' ? 'active' : ''}" onclick="$('[data-${name}-toggle]').addClass('hidden').val(''); $('[data-${name}-input]').attr('disabled', false);">
      <input type="radio" name="${name}.searchType" value="AFTER" ${value.searchType == 'AFTER' ? 'checked' : ''}/> After
    </label>
    <label class="btn btn-custom btn-label ${value.searchType == 'BETWEEN' ? 'active' : ''}" style="margin-right: 0;" onclick="$('[data-${name}-toggle]').removeClass('hidden'); $('[data-${name}-input]').attr('disabled', false);">
      <input type="radio" name="${name}.searchType" value="BETWEEN" ${value.searchType == 'BETWEEN' ? 'checked' : ''}/> Between
    </label>
    <c:if test="${showNone}">
      <label class="btn btn-custom btn-label ${value.searchType == 'NONE' ? 'active' : ''}" onclick="$('[data-${name}-toggle]').addClass('hidden').val(''); $('[data-${name}-input]').attr('disabled', true).val('');">
        <input type="radio" name="${name}.searchType" value="NONE" ${value.searchType == 'NONE' ? 'checked' : ''}/> None
      </label>
    </c:if>
  </div>
</span>
<div class="input-group" style="width: 100%;">
  <input type="text" class="form-control hasDatePicker" name="${name}.date1" value="${dateFormatter.format(value.date1)}" data-${name}-input>
  <span class="input-group-addon ${value.searchType == 'BETWEEN' ? '' : 'hidden'}" data-${name}-toggle>-</span>
  <input type="text" class="form-control hasDatePicker ${value.searchType == 'BETWEEN' ? '' : 'hidden'}" name="${name}.date2" value="${dateFormatter.format(value.date2)}" data-${name}-toggle>
</div>