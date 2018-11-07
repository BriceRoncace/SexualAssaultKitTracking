package gov.idaho.isp.saktrack.domain.search;

import gov.idaho.isp.saktrack.domain.ChainOfCustodyEvent.EventType;
import gov.idaho.isp.saktrack.domain.LawEnforcementDetails.NonSubmissionReason;
import gov.idaho.isp.saktrack.domain.MedicalDetails.VictimType;
import gov.idaho.isp.saktrack.domain.YesNoNa;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;
import org.apache.commons.lang3.StringUtils;

public class SexualAssaultKitSearchCriteria {
  private String serialNumber;
  private Boolean missingEvents;

  //current assignment
  private Long currentJurisdictionId;
  private Long currentAgencyId;

  // event
  private EventType eventType;
  private Long eventOrganization;
  private List<Long> eventOrganizations = new ArrayList<>();
  private String eventActor;
  private CriteriaDate eventDate;
  private Boolean leReceivedCollectedKit;

  // medical
  private VictimType victimType;
  private CriteriaDate collectedDate;
  private Boolean hasCollectedDate;
  private Long requestingLeAgencyId;
  private Boolean requestingLeAgencyNotNull;

  // law
  private String leCaseNumber;
  private String investigator;
  private String crime;
  private CriteriaDate crimeDate;
  private Boolean meetsSubmissionCriteria;
  private NonSubmissionReason nonSubmissionReason;
  private Boolean prosecutorAgrees;
  private Long jurisdictionId;
  private Long reviewingProsecutorOrganization;
  private CriteriaDate plannedDestructionDate;

  // lab
  private String labCaseNumber;
  private CriteriaDate completedDate;
  private Boolean completedDateIsNull;
  private YesNoNa dnaDatabaseEntry;
  private CriteriaDate dnaDatabaseHitDate;
  private CriteriaDate expungedDate;

  public String getSerialNumber() {
    return serialNumber;
  }

  public void setSerialNumber(String serialNumber) {
    this.serialNumber = serialNumber;
  }

  public Boolean getMissingEvents() {
    return missingEvents;
  }

  public void setMissingEvents(Boolean missingEvents) {
    this.missingEvents = missingEvents;
  }

  public Long getCurrentJurisdictionId() {
    return currentJurisdictionId;
  }

  public void setCurrentJurisdictionId(Long currentJurisdictionId) {
    this.currentJurisdictionId = currentJurisdictionId;
  }

  public Long getCurrentAgencyId() {
    return currentAgencyId;
  }

  public void setCurrentAgencyId(Long currentAgencyId) {
    this.currentAgencyId = currentAgencyId;
  }

  public EventType getEventType() {
    return eventType;
  }

  public void setEventType(EventType eventType) {
    this.eventType = eventType;
  }

  public Long getEventOrganization() {
    return eventOrganization;
  }

  public void setEventOrganization(Long eventOrganization) {
    this.eventOrganization = eventOrganization;
  }

  public List<Long> getEventOrganizations() {
    return eventOrganizations;
  }

  public void setEventOrganizations(List<Long> eventOrganizations) {
    this.eventOrganizations = eventOrganizations;
  }

  public String getEventActor() {
    return eventActor;
  }

  public void setEventActor(String eventActor) {
    this.eventActor = eventActor;
  }

  public CriteriaDate getEventDate() {
    return eventDate;
  }

  public void setEventDate(CriteriaDate eventDate) {
    this.eventDate = eventDate;
  }

  public Boolean getLeReceivedCollectedKit() {
    return leReceivedCollectedKit;
  }

  public void setLeReceivedCollectedKit(Boolean leReceivedCollectedKit) {
    this.leReceivedCollectedKit = leReceivedCollectedKit;
  }

  public VictimType getVictimType() {
    return victimType;
  }

  public void setVictimType(VictimType victimType) {
    this.victimType = victimType;
  }

  public CriteriaDate getCollectedDate() {
    return collectedDate;
  }

  public void setCollectedDate(CriteriaDate collectedDate) {
    this.collectedDate = collectedDate;
  }

  public Boolean getHasCollectedDate() {
    return hasCollectedDate;
  }

  public void setHasCollectedDate(Boolean hasCollectedDate) {
    this.hasCollectedDate = hasCollectedDate;
  }

  public Long getRequestingLeAgencyId() {
    return requestingLeAgencyId;
  }

  public void setRequestingLeAgencyId(Long requestingLeAgencyId) {
    this.requestingLeAgencyId = requestingLeAgencyId;
  }

  public Boolean getRequestingLeAgencyNotNull() {
    return requestingLeAgencyNotNull;
  }

  public void setRequestingLeAgencyNotNull(Boolean requestingLeAgencyNotNull) {
    this.requestingLeAgencyNotNull = requestingLeAgencyNotNull;
  }

  public String getLeCaseNumber() {
    return leCaseNumber;
  }

  public void setLeCaseNumber(String leCaseNumber) {
    this.leCaseNumber = leCaseNumber;
  }

  public String getInvestigator() {
    return investigator;
  }

  public void setInvestigator(String investigator) {
    this.investigator = investigator;
  }

  public String getCrime() {
    return crime;
  }

  public void setCrime(String crime) {
    this.crime = crime;
  }

  public CriteriaDate getCrimeDate() {
    return crimeDate;
  }

  public void setCrimeDate(CriteriaDate crimeDate) {
    this.crimeDate = crimeDate;
  }

  public Boolean getMeetsSubmissionCriteria() {
    return meetsSubmissionCriteria;
  }

  public void setMeetsSubmissionCriteria(Boolean meetsSubmissionCriteria) {
    this.meetsSubmissionCriteria = meetsSubmissionCriteria;
  }

  public NonSubmissionReason getNonSubmissionReason() {
    return nonSubmissionReason;
  }

  public void setNonSubmissionReason(NonSubmissionReason nonSubmissionReason) {
    this.nonSubmissionReason = nonSubmissionReason;
  }

  public Boolean getProsecutorAgrees() {
    return prosecutorAgrees;
  }

  public void setProsecutorAgrees(Boolean prosecutorAgrees) {
    this.prosecutorAgrees = prosecutorAgrees;
  }

  public Long getJurisdictionId() {
    return jurisdictionId;
  }

  public void setJurisdictionId(Long jurisdictionId) {
    this.jurisdictionId = jurisdictionId;
  }

  public Long getReviewingProsecutorOrganization() {
    return reviewingProsecutorOrganization;
  }

  public void setReviewingProsecutorOrganization(Long reviewingProsecutorOrganization) {
    this.reviewingProsecutorOrganization = reviewingProsecutorOrganization;
  }

  public CriteriaDate getPlannedDestructionDate() {
    return plannedDestructionDate;
  }

  public void setPlannedDestructionDate(CriteriaDate plannedDestructionDate) {
    this.plannedDestructionDate = plannedDestructionDate;
  }

  public String getLabCaseNumber() {
    return labCaseNumber;
  }

  public void setLabCaseNumber(String labCaseNumber) {
    this.labCaseNumber = labCaseNumber;
  }

  public CriteriaDate getCompletedDate() {
    return completedDate;
  }

  public void setCompletedDate(CriteriaDate completedDate) {
    this.completedDate = completedDate;
  }

  public Boolean getCompletedDateIsNull() {
    return completedDateIsNull;
  }

  public void setCompletedDateIsNull(Boolean completedDateIsNull) {
    this.completedDateIsNull = completedDateIsNull;
  }

  public YesNoNa getDnaDatabaseEntry() {
    return dnaDatabaseEntry;
  }

  public void setDnaDatabaseEntry(YesNoNa dnaDatabaseEntry) {
    this.dnaDatabaseEntry = dnaDatabaseEntry;
  }

  public CriteriaDate getDnaDatabaseHitDate() {
    return dnaDatabaseHitDate;
  }

  public void setDnaDatabaseHitDate(CriteriaDate dnaDatabaseHitDate) {
    this.dnaDatabaseHitDate = dnaDatabaseHitDate;
  }

  public CriteriaDate getExpungedDate() {
    return expungedDate;
  }

  public void setExpungedDate(CriteriaDate expungedDate) {
    this.expungedDate = expungedDate;
  }

  public boolean isEmpty() {
    Field[] fields = this.getClass().getDeclaredFields();
    try {
      for (Field f : fields) {
        f.setAccessible(true);
        Object value = f.get(this);
        if (value instanceof String && StringUtils.isNotBlank((String)value) || !(value instanceof String) && value != null) {
          return false;
        }
      }
    }
    catch (SecurityException | IllegalArgumentException | IllegalAccessException ex) {
      throw new RuntimeException(ex);
    }
    return true;
  }

  @Override
  public String toString() {
    return "SexualAssaultKitSearchCriteria{" + "serialNumber=" + serialNumber + ", missingEvents=" + missingEvents + ", currentJurisdictionId=" + currentJurisdictionId + ", currentAgencyId=" + currentAgencyId + ", eventType=" + eventType + ", eventOrganization=" + eventOrganization + ", eventOrganizations=" + eventOrganizations + ", eventActor=" + eventActor + ", eventDate=" + eventDate + ", leReceivedCollectedKit=" + leReceivedCollectedKit + ", victimType=" + victimType + ", collectedDate=" + collectedDate + ", hasCollectedDate=" + hasCollectedDate + ", requestingLeAgencyId=" + requestingLeAgencyId + ", requestingLeAgencyNotNull=" + requestingLeAgencyNotNull + ", leCaseNumber=" + leCaseNumber + ", investigator=" + investigator + ", crime=" + crime + ", crimeDate=" + crimeDate + ", meetsSubmissionCriteria=" + meetsSubmissionCriteria + ", nonSubmissionReason=" + nonSubmissionReason + ", prosecutorAgrees=" + prosecutorAgrees + ", jurisdictionId=" + jurisdictionId + ", reviewingProsecutorOrganization=" + reviewingProsecutorOrganization + ", plannedDestructionDate=" + plannedDestructionDate + ", labCaseNumber=" + labCaseNumber + ", completedDate=" + completedDate + ", completedDateIsNull=" + completedDateIsNull + ", dnaDatabaseEntry=" + dnaDatabaseEntry + ", dnaDatabaseHitDate=" + dnaDatabaseHitDate + ", expungedDate=" + expungedDate + '}';
  }
}