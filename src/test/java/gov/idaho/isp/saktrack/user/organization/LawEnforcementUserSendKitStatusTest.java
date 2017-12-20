package gov.idaho.isp.saktrack.user.organization;

import gov.idaho.isp.saktrack.ChainOfCustodyEvent;
import gov.idaho.isp.saktrack.EventFlag;
import gov.idaho.isp.saktrack.KitStatus;
import gov.idaho.isp.saktrack.LawEnforcementDetails;
import gov.idaho.isp.saktrack.MedicalDetails;
import gov.idaho.isp.saktrack.SexualAssaultKit;
import gov.idaho.isp.saktrack.config.MockObjUtils;
import gov.idaho.isp.saktrack.jurisdiction.Jurisdiction;
import gov.idaho.isp.saktrack.organization.Organization;
import gov.idaho.isp.saktrack.organization.OrganizationType;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Random;
import org.apache.commons.lang3.StringUtils;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

public class LawEnforcementUserSendKitStatusTest {

  private Organization lab;
  private LabUser labUser;
  private Organization lab2;
  private LabUser labUser2;
  private Organization lab3;
  private LabUser labUser3;

  private Organization medical;
  private MedicalUser medicalUser;
  private Organization medical2;
  private MedicalUser medicalUser2;
  private Organization medical3;
  private MedicalUser medicalUser3;

  private Organization le, leRide, le2, le3;
  private LawEnforcementUser leUser, leRideUser, leUser2, leUser3;

  private Organization pa;
  private LegalUser paUser;

  private List<ChainOfCustodyEvent> chain;
  private Random rand = new Random();
  private LocalDate date = LocalDate.of(2016, 10, 1); //Oct 1, 2016
  private LocalDate startDate = LocalDate.of(2016, 10, 1); //Oct 1, 2016
  private LocalDate endDate = LocalDate.of(2016, 10, 31); //Oct 31, 2016


  @Before
  public void setup() {
    Jurisdiction jurisdiction = MockObjUtils.createJurisdiction();
    lab = MockObjUtils.createOrganization(OrganizationType.LAB, jurisdiction);
    labUser = MockObjUtils.createOrganizationUser(lab);
    lab2 = MockObjUtils.createOrganization(OrganizationType.LAB, jurisdiction);
    labUser2 = MockObjUtils.createOrganizationUser(lab2);
    lab3 = MockObjUtils.createOrganization(OrganizationType.LAB, jurisdiction);
    labUser3 = MockObjUtils.createOrganizationUser(lab3);

    medical = MockObjUtils.createOrganization(OrganizationType.MEDICAL, jurisdiction);
    medicalUser = MockObjUtils.createOrganizationUser(medical);
    medical2 = MockObjUtils.createOrganization(OrganizationType.MEDICAL, jurisdiction);
    medicalUser2 = MockObjUtils.createOrganizationUser(medical2);
    medical3 = MockObjUtils.createOrganization(OrganizationType.MEDICAL, jurisdiction);
    medicalUser3 = MockObjUtils.createOrganizationUser(medical3);

    leRide = MockObjUtils.createOrganization(OrganizationType.LAW_ENFORCEMENT, jurisdiction);
    leRideUser = MockObjUtils.createOrganizationUser(leRide);

    le = MockObjUtils.createOrganization(OrganizationType.LAW_ENFORCEMENT, jurisdiction);
    leUser = MockObjUtils.createOrganizationUser(le);
    le2 = MockObjUtils.createOrganization(OrganizationType.LAW_ENFORCEMENT, jurisdiction);
    leUser2 = MockObjUtils.createOrganizationUser(le2);
    le3 = MockObjUtils.createOrganization(OrganizationType.LAW_ENFORCEMENT, jurisdiction);
    leUser3 = MockObjUtils.createOrganizationUser(le3);

    pa = MockObjUtils.createOrganization(OrganizationType.LEGAL, jurisdiction);
    paUser = MockObjUtils.createOrganizationUser(pa);
  }

  //--------------------tests for 'Send Kit' button disable-----------------------------
//    if (user.getType().equals(User.Type.LAW_ENFORCEMENT)) {
//      LawEnforcementUser leUser = (LawEnforcementUser) user;
//      model.addAttribute("disableSendKitButton", (leUser.isRequestingAgencyMyAgency(kit) && !KitUtils.isSubmittableByLe(kit)));
//    }
//    else {
//      model.addAttribute("disableSendKitButton", user.isAdmin());
//    }

  @Test
  public void requestingAgencyNotMyAgencyTest() {
    SexualAssaultKit kitNotMyAgency = kitReviewFinalized("10001", 2);
    //pre-assertions
    Assert.assertTrue(kitNotMyAgency.getMedicalDetails().getRequestingLeAgency().equals(le2));
    Assert.assertTrue(leUser.getOrganization().equals(le));
    Assert.assertFalse(le.equals(le2));
    //test
    Assert.assertFalse(leUser.isRequestingAgencyMyAgency(kitNotMyAgency));
  }

  @Test
  public void isSubmittableByLeYesTest() {
    //setup and preAssertions
    SexualAssaultKit submittableAgencyYes = leInformationCollectionYesGo(kitToRequestedLe("10002"));
    testIsSubmittableByLe(submittableAgencyYes);
    //test
    Assert.assertEquals(KitStatus.READY_TO_SEND_FOR_ANALYSIS, submittableAgencyYes.getStatus());
  }

  @Test
  public void isSubmittableByLeNoAttorneyYesTest() {
    SexualAssaultKit submittableAgencyNoYes = leInformationCollectionNoThenYesGo(kitToRequestedLe("10003"));
    testIsSubmittableByLe(submittableAgencyNoYes);
    //test
    Assert.assertEquals(KitStatus.READY_TO_SEND_FOR_ANALYSIS, submittableAgencyNoYes.getStatus());
  }

  @Test
  public void isSubmittableByLeNoAttorneyNoTest() {
    SexualAssaultKit submittableAgencyNoNo = leInformationCollectionNoGo(kitToRequestedLe("10004"));
    //test
    Assert.assertEquals(KitStatus.WILL_NOT_BE_ANALYZED, submittableAgencyNoNo.getStatus());
  }

  @Test
  public void isSubmittableByLeYesMissingLeDataTest() {
    SexualAssaultKit submittableAgencyYesMissingData = leInformationCollectionYesGo(kitToRequestedLe("10005"));

    //preAssertion
    Assert.assertEquals(KitStatus.READY_TO_SEND_FOR_ANALYSIS, submittableAgencyYesMissingData.getStatus());
    //action
    submittableAgencyYesMissingData.getLeDetails().setCaseNumber(null);
    //test
    Assert.assertEquals(KitStatus.REQUIRES_LE_DATA, submittableAgencyYesMissingData.getStatus());
    submittableAgencyYesMissingData.getLeDetails().setCaseNumber("CaseNumber");

    //preAssertion
    Assert.assertEquals(KitStatus.READY_TO_SEND_FOR_ANALYSIS, submittableAgencyYesMissingData.getStatus());
    //action
    submittableAgencyYesMissingData.getLeDetails().setInvestigator(null);
    //test
    Assert.assertEquals(KitStatus.REQUIRES_LE_DATA, submittableAgencyYesMissingData.getStatus());
    submittableAgencyYesMissingData.getLeDetails().setInvestigator("Bob");

    //preAssertion
    Assert.assertEquals(KitStatus.READY_TO_SEND_FOR_ANALYSIS, submittableAgencyYesMissingData.getStatus());
    //action
    submittableAgencyYesMissingData.getLeDetails().setCrime(null);
    //test
    Assert.assertEquals(KitStatus.REQUIRES_LE_DATA, submittableAgencyYesMissingData.getStatus());
    submittableAgencyYesMissingData.getLeDetails().setCrime("Crime");

    //preAssertion
    Assert.assertEquals(KitStatus.READY_TO_SEND_FOR_ANALYSIS, submittableAgencyYesMissingData.getStatus());
    //action
    submittableAgencyYesMissingData.getLeDetails().setCrimeDate(null);
    //test
    Assert.assertEquals(KitStatus.REQUIRES_LE_DATA, submittableAgencyYesMissingData.getStatus());
    submittableAgencyYesMissingData.getLeDetails().setCrimeDate(LocalDate.now());

    //preAssertion
    Assert.assertEquals(KitStatus.READY_TO_SEND_FOR_ANALYSIS, submittableAgencyYesMissingData.getStatus());
    //action
    submittableAgencyYesMissingData.getLeDetails().setOutsourcedLabName(null);
    //test
    Assert.assertEquals(KitStatus.READY_TO_SEND_FOR_ANALYSIS, submittableAgencyYesMissingData.getStatus());

    //preAssertion
    Assert.assertEquals(KitStatus.READY_TO_SEND_FOR_ANALYSIS, submittableAgencyYesMissingData.getStatus());
    //action
    submittableAgencyYesMissingData.getLeDetails().setPlannedDestructionDate(null);
    //test
    Assert.assertEquals(KitStatus.READY_TO_SEND_FOR_ANALYSIS, submittableAgencyYesMissingData.getStatus()); //should this be set to force destruction date since is req'd to destroy????

    //preAssertion
    Assert.assertEquals(KitStatus.READY_TO_SEND_FOR_ANALYSIS, submittableAgencyYesMissingData.getStatus());
    //action
    submittableAgencyYesMissingData.getLeDetails().setPlannedDestructionNotificationRequested(null);
    //test
    Assert.assertEquals(KitStatus.READY_TO_SEND_FOR_ANALYSIS, submittableAgencyYesMissingData.getStatus());

    //preAssertion
    Assert.assertEquals(KitStatus.READY_TO_SEND_FOR_ANALYSIS, submittableAgencyYesMissingData.getStatus());
    //action
    submittableAgencyYesMissingData.getLeDetails().setMeetsSubmissionCriteria(null);
    //test
    Assert.assertEquals(KitStatus.REQUIRES_LE_DATA, submittableAgencyYesMissingData.getStatus());
    submittableAgencyYesMissingData.getLeDetails().setMeetsSubmissionCriteria(Boolean.TRUE);

    //preAssertion
    Assert.assertEquals(KitStatus.READY_TO_SEND_FOR_ANALYSIS, submittableAgencyYesMissingData.getStatus());
    //action
    submittableAgencyYesMissingData.getLeDetails().setMeetsSubmissionCriteria(Boolean.FALSE);
    submittableAgencyYesMissingData.getLeDetails().setNonSubmissionReason(null);
    //test
    Assert.assertEquals(KitStatus.REQUIRES_RELEASE_FOR_LEGAL_REVIEW, submittableAgencyYesMissingData.getStatus());
    submittableAgencyYesMissingData.getLeDetails().setMeetsSubmissionCriteria(Boolean.TRUE);

    //-------------------Attorney Review------------------------------------------------------------------

    //preAssertion
    submittableAgencyYesMissingData.getLeDetails().setMeetsSubmissionCriteria(Boolean.FALSE);
    submittableAgencyYesMissingData.getLegalDetails().setReviewingOrganization(pa);
    submittableAgencyYesMissingData.getLeDetails().setNonSubmissionReason(LawEnforcementDetails.NonSubmissionReason.NOT_A_CRIME);
    submittableAgencyYesMissingData.getLegalDetails().setReleasedForReview(LocalDate.now().minusDays(2L));
    submittableAgencyYesMissingData.getLegalDetails().setProsecutorAgrees(Boolean.FALSE);
    submittableAgencyYesMissingData.getLegalDetails().setProsecutorNotes("Notes");
    submittableAgencyYesMissingData.getLegalDetails().setReviewingProsecutor("Bob Roberts");
    submittableAgencyYesMissingData.getLegalDetails().setReviewFinalized(LocalDate.now());
    Assert.assertEquals(KitStatus.READY_TO_SEND_FOR_ANALYSIS, submittableAgencyYesMissingData.getStatus());

    //action
    submittableAgencyYesMissingData.getLegalDetails().setReviewingProsecutor(null);
    //test
    Assert.assertEquals(KitStatus.READY_TO_SEND_FOR_ANALYSIS, submittableAgencyYesMissingData.getStatus());
    submittableAgencyYesMissingData.getLegalDetails().setReviewingProsecutor("Bob the Attorney");

    //preAssertion
    Assert.assertEquals(KitStatus.READY_TO_SEND_FOR_ANALYSIS, submittableAgencyYesMissingData.getStatus());
    //action
    submittableAgencyYesMissingData.getLegalDetails().setReviewingOrganization(null);
    //test
    Assert.assertEquals(KitStatus.READY_TO_SEND_FOR_ANALYSIS, submittableAgencyYesMissingData.getStatus());
    submittableAgencyYesMissingData.getLegalDetails().setReviewingOrganization(pa);

    //preAssertion
    Assert.assertEquals(KitStatus.READY_TO_SEND_FOR_ANALYSIS, submittableAgencyYesMissingData.getStatus());
    //action
    submittableAgencyYesMissingData.getLegalDetails().setReleasedForReview(null);
    //test
    Assert.assertEquals(KitStatus.REQUIRES_RELEASE_FOR_LEGAL_REVIEW, submittableAgencyYesMissingData.getStatus());
    submittableAgencyYesMissingData.getLegalDetails().setReleasedForReview(LocalDate.now().minusDays(2L));

    //preAssertion
    Assert.assertEquals(KitStatus.READY_TO_SEND_FOR_ANALYSIS, submittableAgencyYesMissingData.getStatus());
    //action
    submittableAgencyYesMissingData.getLegalDetails().setProsecutorAgrees(null);
    //test
    Assert.assertEquals(KitStatus.READY_TO_SEND_FOR_ANALYSIS, submittableAgencyYesMissingData.getStatus());
    submittableAgencyYesMissingData.getLegalDetails().setProsecutorAgrees(Boolean.FALSE);

    //preAssertion
    Assert.assertEquals(KitStatus.READY_TO_SEND_FOR_ANALYSIS, submittableAgencyYesMissingData.getStatus());
    //action
    submittableAgencyYesMissingData.getLegalDetails().setProsecutorAgrees(Boolean.TRUE);
    //test
    Assert.assertEquals(KitStatus.WILL_NOT_BE_ANALYZED, submittableAgencyYesMissingData.getStatus());
    submittableAgencyYesMissingData.getLegalDetails().setProsecutorAgrees(Boolean.FALSE);

    //preAssertion
    Assert.assertEquals(KitStatus.READY_TO_SEND_FOR_ANALYSIS, submittableAgencyYesMissingData.getStatus());
    //action
    submittableAgencyYesMissingData.getLegalDetails().setProsecutorNotes(null);
    //test
    Assert.assertEquals(KitStatus.READY_TO_SEND_FOR_ANALYSIS, submittableAgencyYesMissingData.getStatus());
    submittableAgencyYesMissingData.getLegalDetails().setProsecutorNotes("Notes");

    //preAssertion
    Assert.assertEquals(KitStatus.READY_TO_SEND_FOR_ANALYSIS, submittableAgencyYesMissingData.getStatus());
    //action
    submittableAgencyYesMissingData.getLegalDetails().setReviewingProsecutor(null);
    //test
    Assert.assertEquals(KitStatus.READY_TO_SEND_FOR_ANALYSIS, submittableAgencyYesMissingData.getStatus());
    submittableAgencyYesMissingData.getLegalDetails().setReviewingProsecutor("Bob Roberts");

    //preAssertion
    Assert.assertEquals(KitStatus.READY_TO_SEND_FOR_ANALYSIS, submittableAgencyYesMissingData.getStatus());
    //action
    submittableAgencyYesMissingData.getLegalDetails().setReviewFinalized(null);
    //test
    Assert.assertEquals(KitStatus.AWAITING_LEGAL_REVIEW, submittableAgencyYesMissingData.getStatus());
    submittableAgencyYesMissingData.getLegalDetails().setReviewFinalized(LocalDate.now());
  }



  private void testIsSubmittableByLe(SexualAssaultKit kit) {
    LawEnforcementDetails leDetails = kit.getLeDetails();

    boolean isLeDataComplete = StringUtils.isNotBlank(leDetails.getCaseNumber())&&
           StringUtils.isNotBlank(leDetails.getInvestigator()) &&
           StringUtils.isNotBlank(leDetails.getCrime()) &&
           (leDetails.getCrimeDate() != null && !leDetails.getCrimeDate().isAfter(LocalDate.now()));
    boolean isReviewCompleteIfNecessary = (Boolean.TRUE.equals(leDetails.getMeetsSubmissionCriteria()) ||
           (Boolean.FALSE.equals(leDetails.getMeetsSubmissionCriteria()) && kit.getLegalDetails().getReviewFinalized() != null));

    boolean isLeDataCompleteAndReviewCompleteIfNecessary = isLeDataComplete && isReviewCompleteIfNecessary;

    boolean isProcessedByLab = KitStatus.ANALYZED.equals(kit.getStatus());

    boolean isUnsubmittableByLe = isUnsubmittableByLe(kit);

    boolean isSubmittableByLe = isLeDataCompleteAndReviewCompleteIfNecessary && !isProcessedByLab && !isUnsubmittableByLe;

    //pre-assertions
    Assert.assertTrue(isLeDataComplete);
    Assert.assertTrue(isReviewCompleteIfNecessary);
    Assert.assertTrue(isLeDataCompleteAndReviewCompleteIfNecessary);
    Assert.assertFalse(isProcessedByLab);
    Assert.assertFalse(isUnsubmittableByLe);
    Assert.assertTrue(isSubmittableByLe);
  }

  private static boolean isUnsubmittableByLe(SexualAssaultKit kit) {
    return kit.getLeDetails() != null && Boolean.FALSE.equals(kit.getLeDetails().getMeetsSubmissionCriteria()) && Boolean.TRUE.equals(kit.getLegalDetails().getProsecutorAgrees());
  }

  private SexualAssaultKit kitReviewFinalized(String serialNumber, int which) {
    SexualAssaultKit kit = createUnusedKit(serialNumber, date.minusDays(20), which);
    kit = sendReceiveKitDateSendEventFromTo(kit, date.minusDays(19), EventFlag.NEW_KIT_SENT_FROM_LAB, selectLab(which), selectLabUser(which), leRide, leRideUser);
    kit = sendReceiveKitDateSendEventFromTo(kit, date.minusDays(17), null, leRide, leRideUser, selectMedical(which), selectMedicalUser(which));
    kit = medicalEvidenceCollection(kit, date.minusDays(15), which);
    kit = moveMedicalToLeToLe(kit, date.minusDays(10), which);
    kit = leInformation(kit, date.minusDays(5));
    kit.getLeDetails().setMeetsSubmissionCriteria(Boolean.FALSE);
    kit.getLeDetails().setNonSubmissionReason(LawEnforcementDetails.NonSubmissionReason.NO_EVIDENCE);
    kit.getLegalDetails().setReviewingProsecutor("Bob the Attorney");
    kit.getLegalDetails().setProsecutorAgrees(Boolean.TRUE);
    kit.getLegalDetails().setProsecutorNotes("This is my note backing up the thing I said in the other thing");
    kit.getLegalDetails().setReviewFinalized(date);
    return kit;
  }

  private SexualAssaultKit kitToRequestedLe(String serialNumber) {
    SexualAssaultKit kit = createUnusedKit(serialNumber, date.minusDays(20), 1);
    kit = sendReceiveKitDateSendEventFromTo(kit, date.minusDays(19), EventFlag.NEW_KIT_SENT_FROM_LAB, selectLab(1), selectLabUser(1), leRide, leRideUser);
    kit = sendReceiveKitDateSendEventFromTo(kit, date.minusDays(17), null, leRide, leRideUser, selectMedical(1), selectMedicalUser(1));
    kit = medicalEvidenceCollection(kit, date.minusDays(15), 1);
    kit = moveMedicalToLeToLe(kit, date.minusDays(10), 1);
    return kit;
  }



  private SexualAssaultKit createUnusedKit(String sn, LocalDate date, int which) {
    SexualAssaultKit kit = new SexualAssaultKit();
    kit.setSerialNumber(sn);
    kit.setExpirationDate(LocalDate.now().plusYears(which));
    kit.setCurrentAssignment(selectLab(which));
    kit.addChainOfCustodyEvent(createEvent(selectLabUser(which), ChainOfCustodyEvent.EventType.CREATE, date, null, null, selectLabUser(which).getOrganization()));
    return kit;
  }

  private SexualAssaultKit sendReceiveKitDateSendEventFromTo(SexualAssaultKit kit, LocalDate date, EventFlag evFlag, Organization forg, OrganizationUser forgUser, Organization torg, OrganizationUser torgUser) {
    kit.addChainOfCustodyEvent(createEvent(forgUser, ChainOfCustodyEvent.EventType.SEND, date, evFlag != null ? evFlag : null, forg, torg));
    kit.setCurrentAssignment(torg);
    kit.addChainOfCustodyEvent(createEvent(torgUser, ChainOfCustodyEvent.EventType.RECEIVE, date.plusDays(1), null, forg, torg));
    return kit;
  }

  private SexualAssaultKit medicalEvidenceCollection(SexualAssaultKit kit, LocalDate date, int which) {
    kit.setMedicalDetails(new MedicalDetails());
    kit.getMedicalDetails().setCollectionDate(date.plusDays(3));
    kit.getMedicalDetails().setRequestingLeAgency(selectLe(which));
    kit.getMedicalDetails().setVictimType(MedicalDetails.VictimType.NAMED);
    return kit;
  }

  private SexualAssaultKit moveMedicalToLeToLe(SexualAssaultKit kit, LocalDate date, int which) {
    kit.addChainOfCustodyEvent(createEvent(selectMedicalUser(which), ChainOfCustodyEvent.EventType.SEND, date.plusDays(1), EventFlag.MEDICAL_SENT_TO_LE, selectMedical(which), leRide));
    kit.addChainOfCustodyEvent(createEvent(leRideUser, ChainOfCustodyEvent.EventType.RECEIVE, date.plusDays(2), null, selectMedical(which), leRide));
    kit.addChainOfCustodyEvent(createEvent(leRideUser, ChainOfCustodyEvent.EventType.SEND, date.plusDays(3), null, leRide, selectLe(which)));
    kit.setCurrentAssignment(selectLe(which));
    kit.addChainOfCustodyEvent(createEvent(leUser, ChainOfCustodyEvent.EventType.RECEIVE, date.plusDays(4), EventFlag.LE_RECEIVED_COLLECTED_KIT, leRide, selectLe(which)));
    return kit;
  }

  private SexualAssaultKit leInformation(SexualAssaultKit kit, LocalDate date) {
    Integer cs = rand.nextInt(999999999) + 1;
    kit.setLeDetails(new LawEnforcementDetails());
    kit.getLeDetails().setCaseNumber(cs.toString());
    kit.getLeDetails().setInvestigator("Joe Cop serial #" + cs.toString());
    kit.getLeDetails().setCrime("Bad things");
    kit.getLeDetails().setCrimeDate(date.minusWeeks(10).minusDays(3));
    kit.getLeDetails().setPlannedDestructionDate(date.plusYears(50));
    return kit;
  }

  private SexualAssaultKit leInformationCollectionYesGo(SexualAssaultKit kit) {
    kit = leInformation(kit, date);
    kit.getLeDetails().setMeetsSubmissionCriteria(Boolean.TRUE);
    return kit;
  }

  private SexualAssaultKit leInformationCollectionNoGo(SexualAssaultKit kit) {
    kit = leInformation(kit, date);
    kit.getLeDetails().setMeetsSubmissionCriteria(Boolean.FALSE);
    kit.getLeDetails().setNonSubmissionReason(LawEnforcementDetails.NonSubmissionReason.NO_EVIDENCE);
    kit.getLegalDetails().setReleasedForReview(date.plusDays(3));
    kit.getLegalDetails().setReviewingProsecutor("Bob the Attorney");
    kit.getLegalDetails().setProsecutorAgrees(Boolean.TRUE);
    kit.getLegalDetails().setProsecutorNotes("This is my note backing up the thing I said in the other thing");
    kit.getLegalDetails().setReviewFinalized(date.plusDays(5));
    return kit;
  }

  private SexualAssaultKit leInformationCollectionNoThenYesGo(SexualAssaultKit kit) {
    kit = leInformation(kit, date);
    kit.getLeDetails().setMeetsSubmissionCriteria(Boolean.FALSE);
    kit.getLeDetails().setNonSubmissionReason(LawEnforcementDetails.NonSubmissionReason.NO_EVIDENCE);
    kit.getLegalDetails().setReleasedForReview(date.plusDays(3));
    kit.getLegalDetails().setReviewingProsecutor("Bob the Attorney");
    kit.getLegalDetails().setProsecutorAgrees(Boolean.FALSE);
    kit.getLegalDetails().setProsecutorNotes("This is my note backing up that I disagreed with the disagreement of the thing in the other thing");
    kit.getLegalDetails().setReviewFinalized(date.plusDays(5));
    return kit;
  }

  private Organization selectLab(int whichOne) {
    if (whichOne == 1) {
      return lab;
    }
    if (whichOne == 2) {
      return lab2;
    }
    return lab3;
  }

  private Organization selectMedical(int whichOne) {
    if (whichOne == 1) {
      return medical;
    }
    if (whichOne == 2) {
      return medical2;
    }
    return medical3;
  }

  private Organization selectLe(int whichOne) {
    if (whichOne == 1) {
      return le;
    }
    if (whichOne == 2) {
      return le2;
    }
    return le3;
  }

  private OrganizationUser selectLabUser(int whichOne) {
    if (whichOne == 1) {
      return labUser;
    }
    if (whichOne == 2) {
      return labUser2;
    }
    return labUser3;
  }

  private OrganizationUser selectMedicalUser(int whichOne) {
    if (whichOne == 1) {
      return medicalUser;
    }
    if (whichOne == 2) {
      return medicalUser2;
    }
    return medicalUser3;
  }

  private OrganizationUser selectLeUser(int whichOne) {
    if (whichOne == 1) {
      return leUser;
    }
    if (whichOne == 2) {
      return leUser2;
    }
    return leUser3;
  }

  private ChainOfCustodyEvent createEvent(OrganizationUser actor, ChainOfCustodyEvent.EventType eventType, LocalDate date, EventFlag flag, Organization from, Organization to) {
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