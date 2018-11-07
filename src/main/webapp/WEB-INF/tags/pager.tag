<%@ taglib prefix="t" tagdir="/WEB-INF/tags" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="fmt" uri = "http://java.sun.com/jsp/jstl/fmt" %>

<%@attribute name="page" required="true" type="org.springframework.data.domain.Page" %>
<%@attribute name="pageVarName" required="false" type="java.lang.String" %>
<%@attribute name="showInfo" required="false" type="java.lang.Boolean" %>

<c:set var="pageName" value="${pageVarName != null ? pageVarName : 'page'}"/>

<c:set var="pagingUrl">
  <t:currenturl excludeParameterName="${pageName}"/>
</c:set>

<div class="row">
  <div class="col-sm-6">
    <c:if test="${showInfo}">
      <span class="small">
        <c:choose>
          <c:when test="${!page.hasContent()}">
            No results found
          </c:when>
          <c:otherwise>
            Showing ${page.number * page.size + 1}-<c:choose><c:when test="${page.isLast()}">${page.totalElements}</c:when><c:otherwise>${page.number * page.size + page.size}</c:otherwise></c:choose> of <fmt:formatNumber value="${page.totalElements}"/>
          </c:otherwise>
        </c:choose>    
      </span>
    </c:if>
  </div>
  
  <c:if test="${page.hasContent()}">
    <div class="col-sm-6 text-right">
      <c:choose>
        <c:when test="${page.hasPrevious()}">
          <a href="${pagingUrl}&${pageName}=${page.number-1}"><i class="glyphicon glyphicon-chevron-left pager-arrow"></i></a>
        </c:when>
        <c:otherwise>
          <i class="glyphicon glyphicon-chevron-left pager-arrow-disabled"></i>
        </c:otherwise>
      </c:choose>

      <span class="small">
        Page 
        <select onchange="location.href='${pagingUrl}&${pageName}=' + this.value;">
          <c:forEach var="i" begin="1" end="${page.totalPages}">
            <option value="${i-1}" ${i == page.number+1 ? 'selected' : ''}>${i}</option>
          </c:forEach>
        </select>
        of <fmt:formatNumber value="${page.totalPages}"/>
      </span>
        
      <c:choose>
        <c:when test="${page.hasNext()}">
          <a href="${pagingUrl}&${pageName}=${page.number+1}"><i class="glyphicon glyphicon-chevron-right pager-arrow"></i></a>
        </c:when>
        <c:otherwise>
          <i class="glyphicon glyphicon-chevron-right pager-arrow-disabled"></i>
        </c:otherwise>
      </c:choose>    
    </div>
  </c:if>
</div>