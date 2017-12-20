package gov.idaho.isp.saktrack.user.organization;

import gov.idaho.isp.saktrack.ChainOfCustodyEvent;
import gov.idaho.isp.saktrack.EventFlag;
import gov.idaho.isp.saktrack.KitStatus;
import gov.idaho.isp.saktrack.LabDetails;
import gov.idaho.isp.saktrack.MedicalDetails;
import gov.idaho.isp.saktrack.SexualAssaultKit;
import gov.idaho.isp.saktrack.config.EventBuilder;
import gov.idaho.isp.saktrack.config.MockObjUtils;
import gov.idaho.isp.saktrack.dto.CreateKitEventDetails;
import gov.idaho.isp.saktrack.dto.EventDetails;
import gov.idaho.isp.saktrack.exception.SexualAssaultKitTrackingException;
import gov.idaho.isp.saktrack.jurisdiction.Jurisdiction;
import gov.idaho.isp.saktrack.organization.Organization;
import gov.idaho.isp.saktrack.organization.OrganizationType;
import gov.idaho.isp.saktrack.service.ValidationService;
import gov.idaho.isp.saktrack.validation.group.ReleaseForReview;
import gov.idaho.isp.saktrack.validation.group.SendKit;
import java.time.LocalDate;
import java.util.Arrays;
import java.util.HashSet;
import org.junit.Assert;
import org.junit.Test;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class TransitionValidationServiceTest {
  private final ValidationService validationService = mock(ValidationService.class);
  private final TransitionValidationService transitionValidationService = new TransitionValidationServiceImpl(validationService);

  private final Jurisdiction jurisdiction = MockObjUtils.createJurisdiction();
  private final Organization labOrg = MockObjUtils.createOrganization(OrganizationType.LAB, jurisdiction);
  private final Organization medicalOrg = MockObjUtils.createOrganization(OrganizationType.MEDICAL, jurisdiction);
  private final Organization lawOrg = MockObjUtils.createOrganization(OrganizationType.LAW_ENFORCEMENT, jurisdiction);
  private final Organization legalOrg = MockObjUtils.createOrganization(OrganizationType.LEGAL, jurisdiction);

  private final LabUser labUser = MockObjUtils.createOrganizationUser(labOrg);
  private final MedicalUser medUser = MockObjUtils.createOrganizationUser(medicalOrg);
  private final LawEnforcementUser lawUser = MockObjUtils.createOrganizationUser(lawOrg);
  private final LegalUser legalUser = MockObjUtils.createOrganizationUser(legalOrg);

  public TransitionValidationServiceTest() {
    labOrg.setId(Long.valueOf(MockObjUtils.getUniqueId()));
    medicalOrg.setId(Long.valueOf(MockObjUtils.getUniqueId()));
    lawOrg.setId(Long.valueOf(MockObjUtils.getUniqueId()));
    legalOrg.setId(Long.valueOf(MockObjUtils.getUniqueId()));

    when(validationService.validateMedicalDetails(any(), any())).thenReturn(new HashSet<>());
    when(validationService.validateLawEnforcementDetails(any(), any())).thenReturn(new HashSet<>());
    when(validationService.validateLabDetails(any(), any())).thenReturn(new HashSet<>());
  }

  @Test
  public void getCreateValidationStrategyTest() {
    CreateKitEventDetails details = MockObjUtils.createCreateKitEventDetails();
    TestMethod method = () -> transitionValidationService.getCreateValidationStrategy(labUser, details);

    labUser.setOrganization(null);
    Assert.assertTrue(testUserKitServiceMethod(method, "Kit cannot be created by a lab user who is not currently assigned to an organization."));
    labUser.setOrganization(labOrg);
  }

  @Test
  public void getReceiveValidationStrategyTest() {
    EventDetails details = MockObjUtils.createEventDetails();
    KitValidationStrategy strategy = transitionValidationService.getReceiveValidationStrategy(labUser, details, labOrg);
    TestMethod method = () -> strategy.validate(null, "nullKit");

    Assert.assertTrue(testUserKitServiceMethod(method, "Serial number " + "nullKit" + " not found."));

    SexualAssaultKit kit = MockObjUtils.createSexualAssaultKit(labOrg);
    method = () -> strategy.validate(kit, kit.getSerialNumber());

    Assert.assertTrue(testUserKitServiceMethod(method, "Kit " + kit.getSerialNumber() + " has an invalid receive date."));

    kit.addChainOfCustodyEvent(new EventBuilder().eventDate(LocalDate.now().minusDays(10L)).eventType(ChainOfCustodyEvent.EventType.CREATE).build());
    kit.setCurrentAssignment(legalOrg);

    Assert.assertTrue(testUserKitServiceMethod(method, null));
  }

  @Test
  public void getSendValidationStrategyBasicTest() {
    EventDetails details = MockObjUtils.createEventDetails();
    TestMethod method = () -> transitionValidationService.getSendValidationStrategy(labUser, details, legalOrg, OrganizationType.MEDICAL);

    Assert.assertTrue(testUserKitServiceMethod(method, "Destination organization is not a " + OrganizationType.MEDICAL.getLabel() + " organization."));

    KitValidationStrategy strategy = transitionValidationService.getSendValidationStrategy(labUser, details, medicalOrg, OrganizationType.MEDICAL);
    method = () -> strategy.validate(null, "nullKit");

    Assert.assertTrue(testUserKitServiceMethod(method, "Serial number " + "nullKit" + " not found."));

    SexualAssaultKit kit = MockObjUtils.createSexualAssaultKit(legalOrg);
    method = () -> strategy.validate(kit, kit.getSerialNumber());

    Assert.assertTrue(testUserKitServiceMethod(method, "Kit " + kit.getSerialNumber() + " not currently assigned to the initiating organization."));

    kit.setCurrentAssignment(labOrg);
    kit.getMedicalDetails().setCollectionDate(null);

    Assert.assertTrue(testUserKitServiceMethod(method, "Kit " + kit.getSerialNumber() + " has an invalid sent date."));

    kit.getMedicalDetails().setCollectionDate(LocalDate.now().minusDays(5L));
    kit.addChainOfCustodyEvent(new EventBuilder().eventDate(LocalDate.now().minusDays(10L)).eventType(ChainOfCustodyEvent.EventType.CREATE).build());

    Assert.assertTrue(testUserKitServiceMethod(method, "Kit " + kit.getSerialNumber() + " has been collected and cannot be sent back to a medical organization."));
  }

  @Test
  public void getSendValidationStrategyMedicalDetailsValidationTest() {
    EventDetails details = MockObjUtils.createEventDetails();
    SexualAssaultKit kit = new SexualAssaultKit();
    KitValidationStrategy strategy = transitionValidationService.getSendValidationStrategy(medUser, details, lawOrg, OrganizationType.LAW_ENFORCEMENT);

    kit.setSerialNumber(String.valueOf(MockObjUtils.getUniqueId()));
    kit.setCurrentAssignment(medicalOrg);
    kit.addChainOfCustodyEvent(new EventBuilder().eventDate(LocalDate.now().minusDays(10L)).eventType(ChainOfCustodyEvent.EventType.CREATE).build());
    kit.getMedicalDetails().setCollectionDate(LocalDate.now());

    when(validationService.validateMedicalDetails(kit.getMedicalDetails(), SendKit.class)).thenReturn(new HashSet<>(Arrays.asList("medical details validation ran and failed")));

    Assert.assertTrue(testUserKitServiceMethod(() -> strategy.validate(kit, kit.getSerialNumber()), "medical details validation ran and failed"));
  }

  @Test
  public void getSendValidationStrategyLawEnforcementDetailsValidationTest() {
    EventDetails details = MockObjUtils.createEventDetails();
    SexualAssaultKit kit = new SexualAssaultKit();
    KitValidationStrategy strategy = transitionValidationService.getSendValidationStrategy(lawUser, details, labOrg, OrganizationType.LAB);

    kit.setSerialNumber(String.valueOf(MockObjUtils.getUniqueId()));
    kit.setCurrentAssignment(lawOrg);
    kit.addChainOfCustodyEvent(new EventBuilder().eventDate(LocalDate.now().minusDays(10L)).eventType(ChainOfCustodyEvent.EventType.CREATE).build());
    kit.getMedicalDetails().setRequestingLeAgency(lawOrg);

    when(validationService.validateLawEnforcementDetails(kit.getLeDetails(), SendKit.class)).thenReturn(new HashSet<>(Arrays.asList("le details validation ran and failed")));

    Assert.assertTrue(testUserKitServiceMethod(() -> strategy.validate(kit, kit.getSerialNumber()), "le details validation ran and failed"));
  }

  @Test
  public void labDetailsNotValidatedWhenLeSendsKitToLabForAnalysis() {
    EventDetails details = MockObjUtils.createEventDetails();
    SexualAssaultKit kit = new SexualAssaultKit();
    KitValidationStrategy strategy = transitionValidationService.getSendValidationStrategy(labUser, details, lawOrg, OrganizationType.LAW_ENFORCEMENT);

    kit.setSerialNumber(String.valueOf(MockObjUtils.getUniqueId()));
    kit.setCurrentAssignment(labOrg);
    kit.addChainOfCustodyEvent(new EventBuilder().eventDate(LocalDate.now().minusDays(10L)).eventType(ChainOfCustodyEvent.EventType.CREATE).build());
    kit.getMedicalDetails().setCollectionDate(LocalDate.now().minusDays(8L));
    kit.addChainOfCustodyEvent(new EventBuilder().eventDate(LocalDate.now().minusDays(6L)).eventFlag(EventFlag.LE_RECEIVED_COLLECTED_KIT).build());
    kit.setLeDetails(MockObjUtils.getLawEnforcementDetails(MockObjUtils.getUniqueId()));

    Assert.assertTrue(testUserKitServiceMethod(() -> strategy.validate(kit, kit.getSerialNumber()), null));
    verify(validationService, never()).validateLabDetails(kit.getLabDetails(), SendKit.class);
  }

  @Test
  public void labDetailsValidatedWhenExpected() {
    MedicalDetails medicalDetails = new MedicalDetails();
    medicalDetails.setRequestingLeAgency(lawOrg);

    SexualAssaultKit kit = mock(SexualAssaultKit.class);
    when(kit.getCurrentAssignment()).thenReturn(labOrg);
    when(kit.getMedicalDetails()).thenReturn(medicalDetails);
    when(kit.getSerialNumber()).thenReturn("00001");
    when(kit.getLabDetails()).thenReturn(new LabDetails());
    EventDetails details = MockObjUtils.createEventDetails();
    KitValidationStrategy strategy = transitionValidationService.getSendValidationStrategy(labUser, details, lawOrg, OrganizationType.LAW_ENFORCEMENT);
    when(validationService.validateLabDetails(kit.getLabDetails(), SendKit.class)).thenReturn(new HashSet<>(Arrays.asList("lab details validation ran and failed")));

    when(kit.getStatus()).thenReturn(KitStatus.BEING_ANALYZED);
    Assert.assertTrue(testUserKitServiceMethod(() -> strategy.validate(kit, kit.getSerialNumber()), "lab details validation ran and failed"));

    when(kit.getStatus()).thenReturn(KitStatus.ANALYZED);
    Assert.assertTrue(testUserKitServiceMethod(() -> strategy.validate(kit, kit.getSerialNumber()), "lab details validation ran and failed"));
  }

  @Test
  public void getDestroyValidationStrategyTest() {
    EventDetails details = MockObjUtils.createEventDetails();
    KitValidationStrategy strategy = transitionValidationService.getDestroyValidationStrategy(lawUser, details);
    TestMethod method = () -> strategy.validate(null, "nullKit");

    Assert.assertTrue(testUserKitServiceMethod(method, "Serial number " + "nullKit" + " not found."));

    SexualAssaultKit kit = MockObjUtils.createSexualAssaultKit(legalOrg);
    method = () -> strategy.validate(kit, kit.getSerialNumber());

    Assert.assertTrue(testUserKitServiceMethod(method, "Kit " + kit.getSerialNumber() + " not currently assigned to the initiating organization."));
  }

  @Test
  public void getRepurposeValidationStrategyTest() {
    EventDetails details = MockObjUtils.createEventDetails();
    KitValidationStrategy strategy = transitionValidationService.getRepurposeValidationStrategy(medUser, details);
    TestMethod method = () -> strategy.validate(null, "nullKit");
    Assert.assertTrue(testUserKitServiceMethod(method, "Serial number " + "nullKit" + " not found."));

    SexualAssaultKit kit = MockObjUtils.createSexualAssaultKit(legalOrg);
    method = () -> strategy.validate(kit, kit.getSerialNumber());

    Assert.assertTrue(testUserKitServiceMethod(method, "Kit " + kit.getSerialNumber() + " not currently assigned to the initiating organization."));

    kit.setCurrentAssignment(medicalOrg);
    kit.getMedicalDetails().setCollectionDate(LocalDate.now());
    //change in rules: can repurpose at any time even if after collection
    Assert.assertFalse(testUserKitServiceMethod(method, "Kit " + kit.getSerialNumber() + " is collected and cannot be repurposed."));
  }

  @Test
  public void getReleaseForReviewValidationStrategyTest() {
    SexualAssaultKit kit = new SexualAssaultKit();
    KitValidationStrategy strategy = transitionValidationService.getReleaseForReviewValidationStrategy(lawUser);

    when(validationService.validateLawEnforcementDetails(kit.getLeDetails(), ReleaseForReview.class)).thenReturn(new HashSet<>(Arrays.asList("le details validation for review ran and failed")));

    Assert.assertTrue(testUserKitServiceMethod(() -> strategy.validate(kit, kit.getSerialNumber()), "le details validation for review ran and failed"));
    Assert.assertTrue(testUserKitServiceMethod(() -> strategy.validate(null, kit.getSerialNumber()), "Serial number " + kit.getSerialNumber() + " not found."));
  }

  @Test
  public void getReviewValidationStrategyTest() {
    Assert.assertTrue(testUserKitServiceMethod(() -> transitionValidationService.getReviewValidationStrategy(legalUser, null), "Notes must be provided when completing a legal review."));

    SexualAssaultKit kit = new SexualAssaultKit();
    KitValidationStrategy strategy = transitionValidationService.getReviewValidationStrategy(legalUser, "notes");
    kit.getLeDetails().setMeetsSubmissionCriteria(Boolean.TRUE);

    Assert.assertTrue(testUserKitServiceMethod(() -> strategy.validate(kit, kit.getSerialNumber()), "Kit " + kit.getSerialNumber() + " is not reviewable."));
    Assert.assertTrue(testUserKitServiceMethod(() -> strategy.validate(null, kit.getSerialNumber()), "Serial number " + kit.getSerialNumber() + " not found."));
  }

  private boolean testUserKitServiceMethod(TestMethod testMethod, String expectedResult) {
    try {
      testMethod.test();
    }
    catch (SexualAssaultKitTrackingException ex) {
      return ex.getErrors().stream().anyMatch(s -> s.equals(expectedResult));
    }
    return expectedResult == null;
  }

  @FunctionalInterface
  private interface TestMethod {
    void test();
  }
}
