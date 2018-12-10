<%@page contentType="text/html" pageEncoding="UTF-8"%>
<%@ taglib prefix="t" tagdir="/WEB-INF/tags" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>

<t:page>
  <jsp:attribute name="body">
    <%@include file="includes/admin-nav.jspf" %>
    <%@include file="../../includes/messages.jspf" %>
    
    <h3 class="border-bottom mb-2">System Information <small class="float-right text-muted mt-2 d-none d-sm-block">${activeProfile != 'intranet' ? 'Development' : 'Production'} Mode</small></h3>

    <div class="row mb-3">
      <div class="col-12 col-md-4 bottom15">
        <span class="font-weight-bold">Version</span><br/>
        ${version}
      </div>

      <div class="col-12 col-md-4 bottom15">
        <span class="font-weight-bold">Server</span><br/>
        ${pageContext.servletContext.serverInfo}
        <div class="small text-muted">Servlet ${pageContext.servletContext.majorVersion}.${pageContext.servletContext.minorVersion} / Effective: ${pageContext.servletContext.effectiveMajorVersion}.${pageContext.servletContext.effectiveMinorVersion}</div>
      </div>

      <div class="col-12 col-md-4 bottom15">
        <span class="font-weight-bold">Spring Boot Version</span><br/>
        ${springBootVersion}
      </div>
    </div>
      
    <hr/>  
      
    <h4>System Properties</h4>
    <div class="card">
      <div class="card-header">
        <div class="row font-weight-bold">
          <div class="col-12 col-sm-5">Property</div>
          <div class="col-12 col-sm-7">Value</div>
        </div>
      </div>
      <div class="list-group list-group-flush">
        <c:forEach var="entry" items="${systemProperties}">
          <div class="list-group-item hover-light">
            <div class="row">
              <div class="col-12 col-sm-5 font-italic">${entry.key}</div>
              <div class="col-12 col-sm-7">${entry.value}</div>
            </div>
          </div>
        </c:forEach>
      </div>
    </div>
  </jsp:attribute>  
</t:page>