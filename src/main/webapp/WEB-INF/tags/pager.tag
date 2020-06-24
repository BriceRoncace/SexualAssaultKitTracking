<%@ taglib prefix="t" tagdir="/WEB-INF/tags" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="fmt" uri = "http://java.sun.com/jsp/jstl/fmt" %>

<%@attribute name="page" required="true" type="org.springframework.data.domain.Page" %>
<%@attribute name="pageVarName" required="false" type="java.lang.String" %>
<%@attribute name="showSizeSelect" required="false" type="java.lang.Boolean" %>
<%@attribute name="showInfo" required="false" type="java.lang.Boolean" %>
<%@attribute name="sizeVarName" required="false" type="java.lang.String" %>

<c:set var="pageName" value="${pageVarName != null ? pageVarName : 'page'}"/>
<c:set var="sizeName" value="${sizeVarName != null ? sizeVarName : 'size'}"/>

<c:set var="pagingUrl">
  <t:currenturl excludeParameterNames="${pageName},${sizeName}"/>
</c:set>

<div class="row small" style="font-style: italic; ${page.totalPages == 1 ? 'color: #505b72;' : ''}">
  <div class="col-sm-4" >
    <c:if test="${showInfo}">
      <c:choose>
        <c:when test="${!page.hasContent()}">
          &nbsp;No results found
        </c:when>
        <c:otherwise>
          &nbsp;Showing ${page.number * page.size + 1}&#8211;<c:choose><c:when test="${page.isLast()}">${page.totalElements}</c:when><c:otherwise>${page.number * page.size + page.size}</c:otherwise></c:choose> of <fmt:formatNumber value="${page.totalElements}"/>
        </c:otherwise>
      </c:choose>  
    </c:if>
  </div>
  
  <div class="col-sm-4 text-center">
    <c:if test="${showSizeSelect}">
      Size
      <select style="width:auto; padding:0; height: 22px; vertical-align: baseline" onchange="location.href='${pagingUrl}&${pageName}=0&${sizeName}=' + this.value">
        <c:set var="sizeMatchFound" value="false"/>
        <c:forTokens items="10,20,25,50,100,200,500" delims=", " var="sizeOption">
          <c:if test="${page.size == sizeOption}">
            <c:set var="sizeMatchFound" value="true"/>
          </c:if>
          <c:if test="${page.size < sizeOption && !sizeMatchFound}">
            <option value="${page.size}" selected>${page.size}</option>
            <c:set var="sizeMatchFound" value="true"/>
          </c:if>
          <option value="${sizeOption}" ${page.size == sizeOption ? 'selected' : ''}>${sizeOption}</option>
        </c:forTokens>
      </select>
    </c:if>
  </div>
    
  <div class="col-sm-4 text-right">
    <c:if test="${page.hasContent()}">
      <c:choose>
        <c:when test="${page.hasPrevious()}">
          <a href="${pagingUrl}&${pageName}=${page.number-1}&${sizeName}=${page.size}"><i class="glyphicon glyphicon-chevron-left pager-arrow"></i></a>
        </c:when>
        <c:otherwise>
          <i class="glyphicon glyphicon-chevron-left pager-arrow-disabled"></i>
        </c:otherwise>  
      </c:choose>

      Page 
      <select style="width:auto; height: 22px; padding:0; vertical-align: baseline" onchange="location.href='${pagingUrl}&${pageName}=' + this.value + '&${sizeName}=${page.size}'">
        <c:forEach var="i" begin="1" end="${page.totalPages}">
          <option value="${i-1}" ${i == page.number+1 ? 'selected' : ''}>${i}</option>
        </c:forEach>
      </select>
      of <fmt:formatNumber value="${page.totalPages}"/>
        
      <c:choose>
        <c:when test="${page.hasNext()}">
          <a href="${pagingUrl}&${pageName}=${page.number+1}&${sizeName}=${page.size}"><i class="glyphicon glyphicon-chevron-right pager-arrow"></i></a>
        </c:when>
        <c:otherwise>
          <i class="glyphicon glyphicon-chevron-right pager-arrow-disabled"></i>
        </c:otherwise>
      </c:choose>    
    </c:if>
  </div>
</div>