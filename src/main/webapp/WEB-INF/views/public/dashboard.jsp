<%@page contentType="text/html" pageEncoding="UTF-8"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="t" tagdir="/WEB-INF/tags" %>
<t:page>
  <jsp:attribute name="head">
    <link href="<c:url value="/assets/css/pagination.css"/>" rel="stylesheet" type="text/css" />
  </jsp:attribute>
  <jsp:attribute name="body">
    <%@include file="includes/public-nav.jspf" %>
    <%@include file="../../includes/messages.jspf" %>
    <h3 class="text-center"><b>This site tracks sexual assault kits issued by the State of Idaho</b></h3>
    <div class="row">
      <div class="col-sm-3">
        <img class="resize" src="<c:url value="/assets/images/sealcolor_237w.png"/>" alt="Great Seal Of the State of Idaho"/>
      </div>
      <div class="col-sm-6">
        <h3 class="text-center">Authorizing Legislation</h3>
        <p>This website enables the tracking of sexual assault evidence kits* in the state of Idaho in compliance with <a href="https://legislature.idaho.gov/statutesrules/idstat/Title67/T67CH29/SECT67-2919">Idaho Code Chapter 29, Title 67, Section 67-2919.</a></p>
        <p style="font-style: italic"><small>*”Sexual assault evidence kit” means a set of materials, such as swabs and tools for collecting blood samples, used to gather forensic evidence from a victim of reported sexual assault and the evidence obtained with such materials.</small></p>
        <p></p>
        <p><a href="https://isp.idaho.gov/forensics/documents/centrallyStoredData/forms/Sexual%20Assault%20Victim%20Notification%20Form%20and%20Waiver.pdf">Sexual Assault Victim Notification Form and Waiver</a></p>
      </div>
      <div class="col-sm-3 pull-right">
        <img class="resize" src="<c:url value="/assets/images/kit_277w.png"/>" alt="Idaho State Police Forensic Services"/>
      </div>
    </div>
    <div class="col-sm-12">
      <h3>Instructions</h3>
      <p>Victims of sexual assault can view the history and current status of their sexual assault evidence kit by entering the sexual assault evidence kit tracking # in the “serial number” box above and clicking the adjacent search button with the magnifying glass icon (<span class="glyphicon glyphicon-search" style='font-size: 0.8em;'></span>)</p>
      <p>Authorized medical, law enforcement, county prosecutor, and lab personnel should select “Login” and enter their unique Username and Password to manage the status of sexual assault evidence kits under the jurisdiction of their agency.</p>
    </div>
  </jsp:attribute>
</t:page>
