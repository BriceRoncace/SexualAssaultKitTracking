<%@page contentType="text/html" pageEncoding="UTF-8"%>
<%@ taglib prefix="t" tagdir="/WEB-INF/tags" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>

<t:page>
  <jsp:attribute name="head">
    <link href="<c:url value="/assets/css/lazyYT.css"/>" rel="stylesheet" type="text/css" />
  </jsp:attribute>
  <jsp:attribute name="body">
    <%@include file="includes/admin-nav.jspf" %>
    <%@include file="../../includes/messages.jspf" %>
    <h2 class="line-under">Help</h2>
    <%@include file="../../includes/system-admin-contact-info.jspf" %>
    
    <h3 class="line-under">Organization User Actions</h3>
    <div class="row">
      <div class="col-sm-4">
        <b>Self Registration</b>
        <div data-youtube-id="HzFHpn0CDOA" data-title="Self Registration">loading...</div>
      </div>
      <div class="col-sm-4">
        <b>Creating Kits (Lab user)</b>
        <div data-youtube-id="-0EhLto4UAc" data-title="Creating New Kits as Lab User">loading...</div>
      </div>
      <div class="col-sm-4">
        <b>Medical Processing (Medical user)</b>
        <div data-youtube-id="y7Z6IZZ9ojU" data-title="Medical User Workflow">loading...</div>
      </div>
    </div>
    
    <div class="row top25">
      <div class="col-sm-4">
        <b>Law Enforcement / Prosecutor Processing</b>
        <div data-youtube-id="MKvQjxE5sBY" data-title="Law Enforcement User Workflow #1">loading...</div>
      </div>
      <div class="col-sm-4">
        Analyze Kit (Lab user)</b>
        <div data-youtube-id="_aaEVaWRQ2g" data-title="Analyzing Kits as Lab User">loading...</div>
      </div>
      <div class="col-sm-4">
        Law Enforcement Processing Kit (#2)</b>
        <div data-youtube-id="mePkxC00g1w" data-title="Law Enforcement User Workflow #2">loading...</div>
      </div>
    </div>
    
    <div class="row top25">
      <div class="col-sm-4">
        <b>Law Enforcement Processing Kit (#3)</b>
        <div data-youtube-id="HYJ4u-Ose6s" data-title="Law Enforcement User Workflow #3">loading...</div>
      </div>
      <div class="col-sm-4">
        <b>Advanced Search
        <div data-youtube-id="9BHqVldDL3k" data-title="Advanced Search">loading...</div>
      </div>
      <div class="col-sm-4">
        <b>IKTS Training Overview</b>
        <div data-youtube-id="B8INanW925g" data-title="IKITS Training Overview">loading...</div>
      </div>
    </div>
    <br>
    <h3 class="line-under">Administrator Actions</h3>
    <div class="row">
      <div class="col-sm-4">
        <b>Create New Organizations</b>
        <div data-youtube-id="cVz2iLR0XKI" data-title="Creating new organizations">loading...</div>
      </div>
      <div class="col-sm-4">
        <b>Manage Administrators</b>
        <div data-youtube-id="V1Bh1_8g_bQ" data-title="Managing Admins">loading...</div>
      </div>
      <div class="col-sm-4">
        <b>Administrator Actions</b>
        <div data-youtube-id="Olh7ry6QFrU" data-title="Admin Actions">loading...</div>
      </div>
    </div>
  </jsp:attribute>
  <jsp:attribute name="scripts">
    <script src="<c:url value="/assets/js/lazyYT.js"/>"></script>
    <script type="text/javascript">
      $('[data-youtube-id]').lazyYT();
    </script>
  </jsp:attribute>  
</t:page>