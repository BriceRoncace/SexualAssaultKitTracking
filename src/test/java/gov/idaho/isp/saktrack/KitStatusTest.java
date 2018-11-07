package gov.idaho.isp.saktrack;

import gov.idaho.isp.saktrack.domain.MedicalDetails;
import gov.idaho.isp.saktrack.domain.SexualAssaultKit;
import gov.idaho.isp.saktrack.domain.LabDetails;
import gov.idaho.isp.saktrack.domain.EventFlag;
import gov.idaho.isp.saktrack.domain.YesNoNa;
import gov.idaho.isp.saktrack.domain.KitStatus;
import gov.idaho.isp.saktrack.domain.LawEnforcementDetails;
import gov.idaho.isp.saktrack.domain.ChainOfCustodyEvent;
import gov.idaho.isp.saktrack.domain.ChainOfCustodyEvent.EventType;
import gov.idaho.isp.saktrack.domain.LawEnforcementDetails.NonSubmissionReason;
import gov.idaho.isp.saktrack.config.MockObjUtils;
import gov.idaho.isp.saktrack.domain.jurisdiction.Jurisdiction;
import gov.idaho.isp.saktrack.domain.organization.Organization;
import gov.idaho.isp.saktrack.domain.organization.OrganizationType;
import gov.idaho.isp.saktrack.domain.user.organization.LabUser;
import gov.idaho.isp.saktrack.domain.user.organization.LawEnforcementUser;
import gov.idaho.isp.saktrack.domain.user.organization.MedicalUser;
import gov.idaho.isp.saktrack.domain.user.organization.OrganizationUser;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.Month;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

public class KitStatusTest {
  private Organization lab;
  private LabUser labUser;

  private Organization medical;
  private MedicalUser medicalUser;

  private Organization le;
  private LawEnforcementUser leUser;

  @Before
  public void setup() {
    Jurisdiction jurisdiction = MockObjUtils.createJurisdiction();

    lab = MockObjUtils.createOrganization(OrganizationType.LAB, jurisdiction);
    labUser = MockObjUtils.createOrganizationUser(lab);

    medical = MockObjUtils.createOrganization(OrganizationType.MEDICAL, jurisdiction);
    medicalUser = MockObjUtils.createOrganizationUser(medical);

    le = MockObjUtils.createOrganization(OrganizationType.LAW_ENFORCEMENT, jurisdiction);
    leUser = MockObjUtils.createOrganizationUser(le);
  }

  @Test
  public void unusedKit() {
    SexualAssaultKit kit = new SexualAssaultKit();
    kit.setSerialNumber("123");
    kit.addChainOfCustodyEvent(createEvent(labUser, EventType.CREATE, LocalDate.of(2016, Month.OCTOBER, 30), null, null, labUser.getOrganization()));
    Assert.assertEquals(KitStatus.UNUSED, KitStatus.getStatus(kit));
  }

  @Test
  public void stillAnUnusedKit() {
    SexualAssaultKit kit = new SexualAssaultKit();
    kit.setSerialNumber("123");
    addCreateEvent(kit);
    kit.addChainOfCustodyEvent(createEvent(labUser, EventType.SEND, LocalDate.of(2016, Month.OCTOBER, 31), null, labUser.getOrganization(), medicalUser.getOrganization()));
    Assert.assertEquals(KitStatus.UNUSED, KitStatus.getStatus(kit));
  }

  @Test
  public void collectedKitNotYetReceived() {
    SexualAssaultKit kit = new SexualAssaultKit();
    kit.setSerialNumber("123");
    addCreateEvent(kit);
    transferFromLabToMedical(kit);
    kit.setMedicalDetails(new MedicalDetails());
    kit.getMedicalDetails().setCollectionDate(LocalDate.of(2016, Month.NOVEMBER, 1));
    Assert.assertEquals(KitStatus.COLLECTED_BUT_UNRECEIVED, KitStatus.getStatus(kit));
  }

  @Test
  public void collectedAndSentButNotReceivedKit() {
    SexualAssaultKit kit = new SexualAssaultKit();
    kit.setSerialNumber("123");
    addCreateEvent(kit);
    transferFromLabToMedical(kit);
    addMedicalDetails(kit);
    kit.addChainOfCustodyEvent(createEvent(medicalUser, EventType.SEND, LocalDate.of(2016, Month.NOVEMBER, 2), EventFlag.MEDICAL_SENT_TO_LE, labUser.getOrganization(), leUser.getOrganization()));
    Assert.assertEquals(KitStatus.COLLECTED_BUT_UNRECEIVED, KitStatus.getStatus(kit));
  }

  @Test
  public void repurposedKit() {
    SexualAssaultKit kit = new SexualAssaultKit();
    kit.setSerialNumber("123");
    addCreateEvent(kit);
    transferFromLabToMedical(kit);
    kit.addChainOfCustodyEvent(createEvent(medicalUser, EventType.REPURPOSE, LocalDate.of(2016, Month.NOVEMBER, 10), null, labUser.getOrganization(), null));
    Assert.assertEquals(KitStatus.REPURPOSED, KitStatus.getStatus(kit));
  }

  @Test
  public void requiresLeDataBeforeReleasableForReview() {
    SexualAssaultKit kit = new SexualAssaultKit();
    kit.setSerialNumber("123");
    addCreateEvent(kit);
    transferFromLabToMedical(kit);
    addMedicalDetails(kit);
    transferFromMedicalToLe(kit);
    Assert.assertEquals(KitStatus.REQUIRES_LE_DATA, KitStatus.getStatus(kit));
    kit.getLeDetails().setMeetsSubmissionCriteria(Boolean.FALSE);
    kit.getLeDetails().setNonSubmissionReason(NonSubmissionReason.NOT_A_CRIME);
    Assert.assertEquals(KitStatus.REQUIRES_LE_DATA, KitStatus.getStatus(kit));
  }

  @Test
  public void collectedReceivedKitRequiresLeData() {
    SexualAssaultKit kit = new SexualAssaultKit();
    kit.setSerialNumber("123");
    addCreateEvent(kit);
    transferFromLabToMedical(kit);
    addMedicalDetails(kit);
    transferFromMedicalToLe(kit);
    Assert.assertEquals(KitStatus.REQUIRES_LE_DATA, KitStatus.getStatus(kit));
  }

  @Test
  public void collectedReceivedKitRequiresLeData2() {
    SexualAssaultKit kit = new SexualAssaultKit();
    kit.setSerialNumber("123");
    addCreateEvent(kit);
    transferFromLabToMedical(kit);
    addMedicalDetails(kit);
    transferFromMedicalToLe(kit);

    LawEnforcementDetails leDetails = new LawEnforcementDetails();
    leDetails.setSexualAssaultKit(kit);
    kit.setLeDetails(leDetails);
    Assert.assertEquals(KitStatus.REQUIRES_LE_DATA, KitStatus.getStatus(kit));
  }

  @Test
  public void collectedReceivedKitRequiresLeData3() {
    SexualAssaultKit kit = new SexualAssaultKit();
    kit.setSerialNumber("123");
    addCreateEvent(kit);
    transferFromLabToMedical(kit);
    addMedicalDetails(kit);
    transferFromMedicalToLe(kit);
    addLeDetails(kit);
    kit.getLeDetails().setCrime("");
    Assert.assertEquals(KitStatus.REQUIRES_LE_DATA, KitStatus.getStatus(kit));
  }

  @Test
  public void readyToBeSentToLab() {
    SexualAssaultKit kit = new SexualAssaultKit();
    kit.setSerialNumber("123");
    addCreateEvent(kit);
    transferFromLabToMedical(kit);
    addMedicalDetails(kit);
    transferFromMedicalToLe(kit);
    addLeDetails(kit);
    Assert.assertEquals(KitStatus.READY_TO_SEND_FOR_ANALYSIS, KitStatus.getStatus(kit));
  }

  @Test
  public void requiresReleaseForLegalReview() {
    SexualAssaultKit kit = new SexualAssaultKit();
    kit.setSerialNumber("123");
    addCreateEvent(kit);
    transferFromLabToMedical(kit);
    addMedicalDetails(kit);
    transferFromMedicalToLe(kit);
    addLeDetails(kit);
    kit.getLeDetails().setMeetsSubmissionCriteria(Boolean.FALSE);
    kit.getLeDetails().setNonSubmissionReason(LawEnforcementDetails.NonSubmissionReason.NO_EVIDENCE);
    Assert.assertEquals(KitStatus.REQUIRES_RELEASE_FOR_LEGAL_REVIEW, KitStatus.getStatus(kit));
  }

  @Test
  public void awaitingLegalReview2() {
    SexualAssaultKit kit = new SexualAssaultKit();
    kit.setSerialNumber("123");
    addCreateEvent(kit);
    transferFromLabToMedical(kit);
    addMedicalDetails(kit);
    transferFromMedicalToLe(kit);
    addLeDetails(kit);
    kit.getLeDetails().setMeetsSubmissionCriteria(Boolean.FALSE);
    Assert.assertEquals(KitStatus.REQUIRES_RELEASE_FOR_LEGAL_REVIEW, KitStatus.getStatus(kit));
    kit.getLegalDetails().setReleasedForReview(LocalDate.now());
    Assert.assertEquals(KitStatus.AWAITING_LEGAL_REVIEW, KitStatus.getStatus(kit));
  }

  @Test
  public void attorneyAgreesKitWillNotBeAnalyzed() {
    SexualAssaultKit kit = new SexualAssaultKit();
    kit.setSerialNumber("123");
    addCreateEvent(kit);
    transferFromLabToMedical(kit);
    addMedicalDetails(kit);
    transferFromMedicalToLe(kit);
    addLeDetails(kit);
    kit.getLeDetails().setMeetsSubmissionCriteria(Boolean.FALSE);
    kit.getLegalDetails().setReleasedForReview(LocalDate.now());
    kit.getLegalDetails().setProsecutorAgrees(Boolean.TRUE);
    kit.getLegalDetails().setReviewFinalized(LocalDate.now());
    Assert.assertEquals(KitStatus.WILL_NOT_BE_ANALYZED, KitStatus.getStatus(kit));
  }

  @Test
  public void attorneyDisagreesKitWillBeAnalyzed() {
    SexualAssaultKit kit = new SexualAssaultKit();
    kit.setSerialNumber("123");
    addCreateEvent(kit);
    transferFromLabToMedical(kit);
    addMedicalDetails(kit);
    transferFromMedicalToLe(kit);
    addLeDetails(kit);
    kit.getLeDetails().setMeetsSubmissionCriteria(Boolean.FALSE);
    kit.getLegalDetails().setReleasedForReview(LocalDate.now());
    kit.getLegalDetails().setProsecutorAgrees(Boolean.FALSE);
    kit.getLegalDetails().setReviewFinalized(LocalDate.now());
    Assert.assertEquals(KitStatus.READY_TO_SEND_FOR_ANALYSIS, KitStatus.getStatus(kit));
  }

  @Test
  public void kitEnRouteToLabForAnalysis() {
    SexualAssaultKit kit = new SexualAssaultKit();
    kit.setSerialNumber("123");
    addCreateEvent(kit);
    transferFromLabToMedical(kit);
    addMedicalDetails(kit);
    transferFromMedicalToLe(kit);
    addLeDetails(kit);
    Assert.assertEquals(KitStatus.READY_TO_SEND_FOR_ANALYSIS, KitStatus.getStatus(kit));
    sendFromLeToLab(kit);
    Assert.assertEquals(KitStatus.SENT_TO_BE_ANALYZED, KitStatus.getStatus(kit));
    receieveFromLeToLab(kit);
    Assert.assertEquals(KitStatus.BEING_ANALYZED, KitStatus.getStatus(kit));
  }

  @Test
  public void kitBeingAnalyzed() {
    SexualAssaultKit kit = new SexualAssaultKit();
    kit.setSerialNumber("123");
    addCreateEvent(kit);
    Assert.assertEquals(KitStatus.UNUSED, KitStatus.getStatus(kit));
    transferFromLabToMedical(kit);
    Assert.assertEquals(KitStatus.UNUSED, KitStatus.getStatus(kit));
    addMedicalDetails(kit);
    Assert.assertEquals(KitStatus.COLLECTED_BUT_UNRECEIVED, KitStatus.getStatus(kit));
    transferFromMedicalToLe(kit);
    Assert.assertEquals(KitStatus.REQUIRES_LE_DATA, KitStatus.getStatus(kit));
    addLeDetails(kit);
    kit.getLeDetails().setMeetsSubmissionCriteria(Boolean.FALSE);
    kit.getLegalDetails().setReleasedForReview(LocalDate.now());
    Assert.assertEquals(KitStatus.AWAITING_LEGAL_REVIEW, KitStatus.getStatus(kit));
    kit.getLegalDetails().setProsecutorAgrees(Boolean.FALSE);
    kit.getLegalDetails().setReviewFinalized(LocalDate.now());
    Assert.assertEquals(KitStatus.READY_TO_SEND_FOR_ANALYSIS, KitStatus.getStatus(kit));

    transferFromLeToLab(kit);
    Assert.assertEquals(KitStatus.BEING_ANALYZED, KitStatus.getStatus(kit));
  }

  @Test
  public void kitAnalyzed() {
    SexualAssaultKit kit = new SexualAssaultKit();
    kit.setSerialNumber("123");
    addCreateEvent(kit);
    Assert.assertEquals(KitStatus.UNUSED, KitStatus.getStatus(kit));
    transferFromLabToMedical(kit);
    Assert.assertEquals(KitStatus.UNUSED, KitStatus.getStatus(kit));
    addMedicalDetails(kit);
    Assert.assertEquals(KitStatus.COLLECTED_BUT_UNRECEIVED, KitStatus.getStatus(kit));
    transferFromMedicalToLe(kit);
    Assert.assertEquals(KitStatus.REQUIRES_LE_DATA, KitStatus.getStatus(kit));
    addLeDetails(kit);
    kit.getLeDetails().setMeetsSubmissionCriteria(Boolean.FALSE);
    Assert.assertEquals(KitStatus.REQUIRES_RELEASE_FOR_LEGAL_REVIEW, KitStatus.getStatus(kit));
    kit.getLegalDetails().setReleasedForReview(LocalDate.now());
    Assert.assertEquals(KitStatus.AWAITING_LEGAL_REVIEW, KitStatus.getStatus(kit));
    kit.getLegalDetails().setProsecutorAgrees(Boolean.FALSE);
    kit.getLegalDetails().setReviewFinalized(LocalDate.now());
    Assert.assertEquals(KitStatus.READY_TO_SEND_FOR_ANALYSIS, KitStatus.getStatus(kit));

    transferFromLeToLab(kit);
    Assert.assertEquals(KitStatus.BEING_ANALYZED, KitStatus.getStatus(kit));
    kit.setLabDetails(new LabDetails());
    kit.getLabDetails().setDateCompleted(LocalDate.now());
    Assert.assertEquals(KitStatus.ANALYZED, KitStatus.getStatus(kit));
  }

  @Test
  public void kitAnalyzed2() {
    SexualAssaultKit kit = new SexualAssaultKit();
    kit.setSerialNumber("123");
    addCreateEvent(kit);
    Assert.assertEquals(KitStatus.UNUSED, KitStatus.getStatus(kit));
    transferFromLabToMedical(kit);
    Assert.assertEquals(KitStatus.UNUSED, KitStatus.getStatus(kit));
    addMedicalDetails(kit);
    Assert.assertEquals(KitStatus.COLLECTED_BUT_UNRECEIVED, KitStatus.getStatus(kit));
    transferFromMedicalToLe(kit);
    Assert.assertEquals(KitStatus.REQUIRES_LE_DATA, KitStatus.getStatus(kit));
    addLeDetails(kit);
    kit.getLeDetails().setMeetsSubmissionCriteria(Boolean.FALSE);
    kit.getLegalDetails().setReleasedForReview(LocalDate.now());
    Assert.assertEquals(KitStatus.AWAITING_LEGAL_REVIEW, KitStatus.getStatus(kit));
    kit.getLegalDetails().setProsecutorAgrees(Boolean.FALSE);
    kit.getLegalDetails().setReviewFinalized(LocalDate.now());
    Assert.assertEquals(KitStatus.READY_TO_SEND_FOR_ANALYSIS, KitStatus.getStatus(kit));

    transferFromLeToLab(kit);
    Assert.assertEquals(KitStatus.BEING_ANALYZED, KitStatus.getStatus(kit));
    kit.setLabDetails(new LabDetails());
    kit.getLabDetails().setDateCompleted(LocalDate.now());
    kit.getLabDetails().setCaseNumber("LabCase#");
    kit.getLabDetails().setDnaDatabaseEntry(YesNoNa.NO);
    Assert.assertEquals(KitStatus.ANALYZED, KitStatus.getStatus(kit));
    transferFromLabToLe(kit);
    Assert.assertEquals(KitStatus.ANALYZED, KitStatus.getStatus(kit));
  }

  @Test
  public void kitDestroyed() {
    SexualAssaultKit kit = new SexualAssaultKit();
    kit.setSerialNumber("123");
    addCreateEvent(kit);
    Assert.assertEquals(KitStatus.UNUSED, KitStatus.getStatus(kit));
    transferFromLabToMedical(kit);
    Assert.assertEquals(KitStatus.UNUSED, KitStatus.getStatus(kit));
    addMedicalDetails(kit);
    Assert.assertEquals(KitStatus.COLLECTED_BUT_UNRECEIVED, KitStatus.getStatus(kit));
    transferFromMedicalToLe(kit);
    Assert.assertEquals(KitStatus.REQUIRES_LE_DATA, KitStatus.getStatus(kit));
    addLeDetails(kit);
    kit.getLeDetails().setMeetsSubmissionCriteria(Boolean.FALSE);
    kit.getLegalDetails().setReleasedForReview(LocalDate.now());
    Assert.assertEquals(KitStatus.AWAITING_LEGAL_REVIEW, KitStatus.getStatus(kit));
    kit.getLegalDetails().setProsecutorAgrees(Boolean.FALSE);
    kit.getLegalDetails().setReviewFinalized(LocalDate.now());
    Assert.assertEquals(KitStatus.READY_TO_SEND_FOR_ANALYSIS, KitStatus.getStatus(kit));

    transferFromLeToLab(kit);
    Assert.assertEquals(KitStatus.BEING_ANALYZED, KitStatus.getStatus(kit));
    kit.setLabDetails(new LabDetails());
    kit.getLabDetails().setDateCompleted(LocalDate.now());
    kit.getLabDetails().setCaseNumber("LabCase#");
    kit.getLabDetails().setDnaDatabaseEntry(YesNoNa.NO);
    Assert.assertEquals(KitStatus.ANALYZED, KitStatus.getStatus(kit));
    transferFromLabToLe(kit);
    Assert.assertEquals(KitStatus.ANALYZED, KitStatus.getStatus(kit));
    kit.addChainOfCustodyEvent(createEvent(leUser, EventType.DESTROY, LocalDate.of(2016, Month.DECEMBER, 10), null, leUser.getOrganization(), null));
    Assert.assertEquals(KitStatus.DESTROYED, KitStatus.getStatus(kit));
  }

  // helper methods -------------------------------------------------------------------------------------------------------------------
  private void addCreateEvent(SexualAssaultKit kit) {
    kit.addChainOfCustodyEvent(createEvent(labUser, EventType.CREATE, LocalDate.of(2016, Month.OCTOBER, 30), EventFlag.NEW_KIT_SENT_FROM_LAB, null, labUser.getOrganization()));
  }

  private void transferFromLabToMedical(SexualAssaultKit kit) {
    kit.addChainOfCustodyEvent(createEvent(labUser, EventType.SEND, LocalDate.of(2016, Month.OCTOBER, 31), null, labUser.getOrganization(), medicalUser.getOrganization()));
    kit.addChainOfCustodyEvent(createEvent(medicalUser, EventType.RECEIVE, LocalDate.of(2016, Month.OCTOBER, 31), null, labUser.getOrganization(), medicalUser.getOrganization()));
  }

  private void transferFromMedicalToLe(SexualAssaultKit kit) {
    kit.addChainOfCustodyEvent(createEvent(medicalUser, EventType.SEND, LocalDate.of(2016, Month.NOVEMBER, 2), EventFlag.MEDICAL_SENT_TO_LE, labUser.getOrganization(), leUser.getOrganization()));
    kit.addChainOfCustodyEvent(createEvent(leUser, EventType.RECEIVE, LocalDate.of(2016, Month.NOVEMBER, 2), EventFlag.LE_RECEIVED_COLLECTED_KIT, labUser.getOrganization(), leUser.getOrganization()));
  }

  private void transferFromLeToLab(SexualAssaultKit kit) {
    sendFromLeToLab(kit);
    receieveFromLeToLab(kit);
  }

  private void sendFromLeToLab(SexualAssaultKit kit) {
    kit.addChainOfCustodyEvent(createEvent(leUser, EventType.SEND, LocalDate.of(2016, Month.NOVEMBER, 10), EventFlag.LE_SENT_FOR_ANALYSIS, leUser.getOrganization(), labUser.getOrganization()));
  }

  private void receieveFromLeToLab(SexualAssaultKit kit) {
    kit.addChainOfCustodyEvent(createEvent(labUser, EventType.RECEIVE, LocalDate.of(2016, Month.NOVEMBER, 11), EventFlag.LAB_RECEIVED, leUser.getOrganization(), labUser.getOrganization()));
  }

  private void transferFromLabToLe(SexualAssaultKit kit) {
    kit.addChainOfCustodyEvent(createEvent(labUser, EventType.SEND, LocalDate.of(2016, Month.NOVEMBER, 15), null, labUser.getOrganization(), leUser.getOrganization()));
    kit.addChainOfCustodyEvent(createEvent(leUser, EventType.RECEIVE, LocalDate.of(2016, Month.NOVEMBER, 16), null, labUser.getOrganization(), leUser.getOrganization()));
  }

  private void addMedicalDetails(SexualAssaultKit kit) {
    kit.setMedicalDetails(new MedicalDetails());
    kit.getMedicalDetails().setSexualAssaultKit(kit);
    kit.getMedicalDetails().setRequestingLeAgency(leUser.getOrganization());
    kit.getMedicalDetails().setCollectionDate(LocalDate.of(2016, Month.NOVEMBER, 1));
    kit.getMedicalDetails().setVictimType(MedicalDetails.VictimType.NAMED);
  }

  private void addLeDetails(SexualAssaultKit kit) {
    kit.setLeDetails(new LawEnforcementDetails());
    kit.getLeDetails().setSexualAssaultKit(kit);
    kit.getLeDetails().setCaseNumber("7");
    kit.getLeDetails().setMeetsSubmissionCriteria(Boolean.TRUE);
    kit.getLeDetails().setCrime("crime");
    kit.getLeDetails().setCrimeDate(LocalDate.now());
    kit.getLeDetails().setInvestigator("investigator");
  }

  private ChainOfCustodyEvent createEvent(OrganizationUser actor, EventType eventType, LocalDate date, EventFlag flag, Organization from, Organization to) {
    ChainOfCustodyEvent event = new ChainOfCustodyEvent();
    event.setEventType(eventType);
    event.setEventDate(date);
    event.setDigitalTimestamp(LocalDateTime.now());
    event.setActor(actor.getDisplayName());
    event.setActorOrganization(actor.getOrganization());
    event.setFrom(from);
    event.setTo(to);
    event.setEventFlag(flag);
    return event;
  }
}