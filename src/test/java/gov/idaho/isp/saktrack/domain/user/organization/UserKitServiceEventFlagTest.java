package gov.idaho.isp.saktrack.domain.user.organization;

import gov.idaho.isp.saktrack.config.MockObjUtils;
import gov.idaho.isp.saktrack.domain.ChainOfCustodyEvent;
import gov.idaho.isp.saktrack.domain.ChainOfCustodyEvent.EventType;
import gov.idaho.isp.saktrack.domain.EventFlag;
import gov.idaho.isp.saktrack.domain.KitStatus;
import gov.idaho.isp.saktrack.domain.LabDetails;
import gov.idaho.isp.saktrack.domain.MedicalDetails;
import gov.idaho.isp.saktrack.domain.SexualAssaultKit;
import gov.idaho.isp.saktrack.domain.SexualAssaultKitRepository;
import gov.idaho.isp.saktrack.domain.dto.EventDetails;
import gov.idaho.isp.saktrack.domain.organization.Organization;
import gov.idaho.isp.saktrack.domain.organization.OrganizationRepository;
import gov.idaho.isp.saktrack.domain.organization.OrganizationType;
import gov.idaho.isp.saktrack.exception.SexualAssaultKitTrackingException;
import java.time.LocalDate;
import java.util.Arrays;
import java.util.Optional;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.argThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import org.springframework.context.ApplicationEventPublisher;

/**
 * Integration test testing that when a kit transition operation is invoked on a user
 * that the kit's chain of custody includes the appropriate event flag.
 *
 * This is an integration test because there are more than one non-mock objects being tested:
 * UserKitServiceImpl and the Organization user instances (LabUser, MedicalUser, LawEnforcementUser)
 */
public class UserKitServiceEventFlagTest {
  private final OrganizationRepository orgRepo = mock(OrganizationRepository.class);
  private final SexualAssaultKitRepository sakRepo = mock(SexualAssaultKitRepository.class);
  private final TransitionValidationService transitionValidationService = mock(TransitionValidationService.class);
  private final UserKitService userKitService = new UserKitServiceImpl(orgRepo, sakRepo, mock(ApplicationEventPublisher.class), transitionValidationService);

  private final Organization labOrg = MockObjUtils.createOrganization(OrganizationType.LAB, MockObjUtils.createJurisdiction());
  private final Organization medOrg = MockObjUtils.createOrganization(OrganizationType.MEDICAL, MockObjUtils.createJurisdiction());
  private final Organization lawOrg = MockObjUtils.createOrganization(OrganizationType.LAW_ENFORCEMENT, MockObjUtils.createJurisdiction());
  private final LabUser labUser = MockObjUtils.createOrganizationUser(labOrg);
  private final MedicalUser medUser = MockObjUtils.createOrganizationUser(medOrg);
  private final LawEnforcementUser lawUser = MockObjUtils.createOrganizationUser(lawOrg);

  @Before
  public void testSetup() {
    labUser.setUserKitService(userKitService);
    medUser.setUserKitService(userKitService);
    lawUser.setUserKitService(userKitService);

    labOrg.setId(1L);
    medOrg.setId(2L);
    lawOrg.setId(3L);

    when(orgRepo.findById(labOrg.getId())).thenReturn(Optional.of(labOrg));
    when(orgRepo.findById(medOrg.getId())).thenReturn(Optional.of(medOrg));
    when(orgRepo.findById(lawOrg.getId())).thenReturn(Optional.of(lawOrg));

    KitValidationStrategy defaultStrategy = (SexualAssaultKit kit, String serialNumber) -> {};

    when(transitionValidationService.getCreateValidationStrategy(any(), any())).thenReturn(defaultStrategy);
    when(transitionValidationService.getReceiveValidationStrategy(any(), any(), any())).thenReturn(defaultStrategy);
    when(transitionValidationService.getSendValidationStrategy(any(), any(), any(), any())).thenReturn(defaultStrategy);
  }

  @Test
  public void labToMedicalUnusedKit() {
    SexualAssaultKit kit = mockSexualAssaultKit("00001", labUser, KitStatus.UNUSED);
    ChainOfCustodyEvent create = mockCreateEvent(labUser, LocalDate.now().minusDays(1L));
    when(kit.getChainOfCustody()).thenReturn(Arrays.asList(create));

    EventDetails details = MockObjUtils.createEventDetails();
    details.setSerialNumberList(Arrays.asList("00001"));
    details.setOrgId(medOrg.getId());
    details.setEventDate(LocalDate.now());

    // act
    labUser.batchSendToMedical(details);

    verify(kit).addChainOfCustodyEvent(argThat(coc -> coc.getEventFlag() == EventFlag.NEW_KIT_SENT_FROM_LAB));
  }

  @Test
  public void labToMedicalCollectedKitYieldsNoEventFlag() {
    SexualAssaultKit kit = mockSexualAssaultKit("00001", labUser, KitStatus.ANALYZED);
    ChainOfCustodyEvent create = mockCreateEvent(labUser, LocalDate.now().minusDays(1L));
    when(kit.getChainOfCustody()).thenReturn(Arrays.asList(create));

    MedicalDetails medDetails = mock(MedicalDetails.class);
    when(medDetails.getCollectionDate()).thenReturn(LocalDate.now().minusDays(9L));
    when(kit.getMedicalDetails()).thenReturn(medDetails);

    EventDetails details = MockObjUtils.createEventDetails();
    details.setSerialNumberList(Arrays.asList("00001"));
    details.setOrgId(medOrg.getId());
    details.setEventDate(LocalDate.now());

    // act
    labUser.batchSendToMedical(details);

    verify(kit).addChainOfCustodyEvent(argThat(coc -> coc.getEventFlag() == null));
  }

  @Test
  public void medicalSendsToLeCollectedKit() {
    SexualAssaultKit kit = mockSexualAssaultKit("00001", medUser, KitStatus.COLLECTED_BUT_UNRECEIVED);
    ChainOfCustodyEvent create = mockCreateEvent(labUser, LocalDate.now().minusDays(14L));
    ChainOfCustodyEvent receiveByMed = mockReceiveEvent(medUser, labOrg, LocalDate.now().minusDays(10L));
    when(kit.getChainOfCustody()).thenReturn(Arrays.asList(create, receiveByMed));

    MedicalDetails medDetails = mock(MedicalDetails.class);
    when(medDetails.getRequestingLeAgency()).thenReturn(lawOrg);
    when(medDetails.getCollectionDate()).thenReturn(LocalDate.now().minusDays(9L));
    when(kit.getMedicalDetails()).thenReturn(medDetails);

    EventDetails details = MockObjUtils.createEventDetails();
    details.setSerialNumberList(Arrays.asList("00001"));
    details.setOrgId(lawOrg.getId());
    details.setEventDate(LocalDate.now());

    try {
      // act
      medUser.batchSendToLawEnforcement(details);
      verify(kit).addChainOfCustodyEvent(argThat(coc -> coc.getEventFlag() == EventFlag.MEDICAL_SENT_TO_LE));
    }
    catch (SexualAssaultKitTrackingException ex) {
      Assert.fail(ex.getErrors().toString());
    }
  }

  @Test
  public void lawReceivesCollectedKit() {
    SexualAssaultKit kit = mockSexualAssaultKit("00001", lawUser, KitStatus.COLLECTED_BUT_UNRECEIVED);
    ChainOfCustodyEvent create = mockCreateEvent(labUser, LocalDate.now().minusDays(14L));
    ChainOfCustodyEvent receiveByMed = mockReceiveEvent(medUser, labUser.getOrganization(), LocalDate.now().minusDays(10L));
    ChainOfCustodyEvent sentByMed = mockSendEvent(medUser, lawOrg, LocalDate.now().minusDays(1L));
    when(kit.getChainOfCustody()).thenReturn(Arrays.asList(create, receiveByMed, sentByMed));

    MedicalDetails medDetails = mock(MedicalDetails.class);
    when(medDetails.getRequestingLeAgency()).thenReturn(lawOrg);
    when(medDetails.getCollectionDate()).thenReturn(LocalDate.now().minusDays(9L));
    when(kit.getMedicalDetails()).thenReturn(medDetails);

    EventDetails details = MockObjUtils.createEventDetails();
    details.setSerialNumberList(Arrays.asList("00001"));
    details.setOrgId(medOrg.getId());
    details.setEventDate(LocalDate.now());

    try {
      // act
      lawUser.batchReceive(details);
      verify(kit).addChainOfCustodyEvent(argThat(coc -> coc.getEventFlag() == EventFlag.LE_RECEIVED_COLLECTED_KIT));
    }
    catch (SexualAssaultKitTrackingException ex) {
      Assert.fail(ex.getErrors().toString());
    }
  }

  @Test
  public void lawSendsToLabForAnalysis() {
    SexualAssaultKit kit = mockSexualAssaultKit("00001", lawUser, KitStatus.READY_TO_SEND_FOR_ANALYSIS);
    ChainOfCustodyEvent create = mockCreateEvent(labUser, LocalDate.now().minusDays(14L));
    ChainOfCustodyEvent receiveByMed = mockReceiveEvent(medUser, labOrg, LocalDate.now().minusDays(10L));
    ChainOfCustodyEvent sentByMed = mockSendEvent(medUser, lawOrg, LocalDate.now().minusDays(9L));
    ChainOfCustodyEvent receiveByLaw = mockReceiveEvent(lawUser, medUser.getOrganization(), LocalDate.now().minusDays(8L));
    when(kit.getChainOfCustody()).thenReturn(Arrays.asList(create, receiveByMed, sentByMed, receiveByLaw));

    MedicalDetails medDetails = mock(MedicalDetails.class);
    when(medDetails.getRequestingLeAgency()).thenReturn(lawOrg);
    when(kit.getMedicalDetails()).thenReturn(medDetails);

    EventDetails details = MockObjUtils.createEventDetails();
    details.setSerialNumberList(Arrays.asList("00001"));
    details.setOrgId(labOrg.getId());
    details.setEventDate(LocalDate.now());

    try {
      // act
      lawUser.batchSendToLab(details);
      verify(kit).addChainOfCustodyEvent(argThat(coc -> coc.getEventFlag() == EventFlag.LE_SENT_FOR_ANALYSIS));
    }
    catch (SexualAssaultKitTrackingException ex) {
      Assert.fail(ex.getErrors().toString());
    }
  }

  @Test
  public void lawSendsToLaw() {
    SexualAssaultKit kit = mockSexualAssaultKit("00001", lawUser, KitStatus.READY_TO_SEND_FOR_ANALYSIS);
    ChainOfCustodyEvent create = mockCreateEvent(labUser, LocalDate.now().minusDays(14L));
    ChainOfCustodyEvent receiveByMed = mockReceiveEvent(medUser, labOrg, LocalDate.now().minusDays(10L));
    ChainOfCustodyEvent sentByMed = mockSendEvent(medUser, lawOrg, LocalDate.now().minusDays(9L));
    ChainOfCustodyEvent receiveByLaw = mockReceiveEvent(lawUser, medUser.getOrganization(), LocalDate.now().minusDays(8L));
    when(kit.getChainOfCustody()).thenReturn(Arrays.asList(create, receiveByMed, sentByMed, receiveByLaw));

    MedicalDetails medDetails = mock(MedicalDetails.class);
    when(medDetails.getRequestingLeAgency()).thenReturn(lawOrg);
    when(kit.getMedicalDetails()).thenReturn(medDetails);

    EventDetails details = MockObjUtils.createEventDetails();
    details.setSerialNumberList(Arrays.asList("00001"));
    details.setOrgId(lawOrg.getId());
    details.setEventDate(LocalDate.now());

    try {
      // act
      lawUser.batchSendToLawEnforcement(details);
      verify(kit).addChainOfCustodyEvent(argThat(coc -> coc.getEventFlag() == EventFlag.LE_SENT_FOR_ANALYSIS));
    }
    catch (SexualAssaultKitTrackingException ex) {
      Assert.fail(ex.getErrors().toString());
    }
  }

  @Test
  public void labReceivesKitToAnalyze() {
    SexualAssaultKit kit = mockSexualAssaultKit("00001", labUser, KitStatus.SENT_TO_BE_ANALYZED);
    ChainOfCustodyEvent create = mockCreateEvent(labUser, LocalDate.now().minusDays(14L));
    ChainOfCustodyEvent receiveByMed = mockReceiveEvent(medUser, labOrg, LocalDate.now().minusDays(10L));
    ChainOfCustodyEvent sentByMed = mockSendEvent(medUser, lawOrg, LocalDate.now().minusDays(9L));
    ChainOfCustodyEvent receiveByLaw = mockReceiveEvent(lawUser, medUser.getOrganization(), LocalDate.now().minusDays(8L));
    ChainOfCustodyEvent sentByLaw = mockReceiveEvent(lawUser, labUser.getOrganization(), LocalDate.now().minusDays(7L));
    when(kit.getChainOfCustody()).thenReturn(Arrays.asList(create, receiveByMed, sentByMed, receiveByLaw, sentByLaw));

    MedicalDetails medDetails = mock(MedicalDetails.class);
    when(medDetails.getRequestingLeAgency()).thenReturn(lawOrg);
    when(medDetails.getCollectionDate()).thenReturn(LocalDate.now().minusDays(9L));
    when(kit.getMedicalDetails()).thenReturn(medDetails);

    EventDetails details = MockObjUtils.createEventDetails();
    details.setSerialNumberList(Arrays.asList("00001"));
    details.setOrgId(lawOrg.getId());
    details.setEventDate(LocalDate.now());

    try {
      // act
      labUser.batchReceive(details);
      verify(kit).addChainOfCustodyEvent(argThat(coc -> coc.getEventFlag() == EventFlag.LAB_RECEIVED));
    }
    catch (SexualAssaultKitTrackingException ex) {
      Assert.fail(ex.getErrors().toString());
    }
  }

  @Test
  public void leReceivesAnalyzedKit() {
    SexualAssaultKit kit = mockSexualAssaultKit("00001", lawUser, KitStatus.ANALYZED);
    ChainOfCustodyEvent create = mockCreateEvent(labUser, LocalDate.now().minusDays(14L));
    ChainOfCustodyEvent receiveByMed = mockReceiveEvent(medUser, labOrg, LocalDate.now().minusDays(10L));
    ChainOfCustodyEvent sentByMed = mockSendEvent(medUser, lawOrg, LocalDate.now().minusDays(9L));
    ChainOfCustodyEvent receiveByLaw = mockReceiveEvent(lawUser, medUser.getOrganization(), LocalDate.now().minusDays(8L));
    ChainOfCustodyEvent sentByLaw = mockReceiveEvent(lawUser, labUser.getOrganization(), LocalDate.now().minusDays(7L));
    ChainOfCustodyEvent receiveByLab = mockReceiveEvent(labUser, labUser.getOrganization(), LocalDate.now().minusDays(6L));
    ChainOfCustodyEvent sentByLab = mockSendEvent(labUser, lawUser.getOrganization(), LocalDate.now().minusDays(5L));
    when(kit.getChainOfCustody()).thenReturn(Arrays.asList(create, receiveByMed, sentByMed, receiveByLaw, sentByLaw, receiveByLab, sentByLab));

    MedicalDetails medDetails = mock(MedicalDetails.class);
    when(medDetails.getRequestingLeAgency()).thenReturn(lawOrg);
    when(medDetails.getCollectionDate()).thenReturn(LocalDate.now().minusDays(9L));
    when(kit.getMedicalDetails()).thenReturn(medDetails);

    LabDetails labDetails = mock(LabDetails.class);
    when(labDetails.getDateCompleted()).thenReturn((LocalDate.now().minusDays(2L)));
    when(kit.getLabDetails()).thenReturn(labDetails);

    EventDetails details = MockObjUtils.createEventDetails();
    details.setSerialNumberList(Arrays.asList("00001"));
    details.setOrgId(lawOrg.getId());
    details.setEventDate(LocalDate.now());

    try {
      // act
      lawUser.batchReceive(details);
      verify(kit).addChainOfCustodyEvent(argThat(coc -> coc.getEventFlag() == EventFlag.LE_RECEIVED_ANALYZED_KIT));
    }
    catch (SexualAssaultKitTrackingException ex) {
      Assert.fail(ex.getErrors().toString());
    }
  }

  private SexualAssaultKit mockSexualAssaultKit(String serialNumber, OrganizationUser owner, KitStatus status) {
    SexualAssaultKit kit = mock(SexualAssaultKit.class);
    when(kit.getCurrentAssignment()).thenReturn(owner.getOrganization());
    when(kit.getStatus()).thenReturn(status);
    when(kit.getSerialNumber()).thenReturn(serialNumber);
    when(sakRepo.findBySerialNumber(serialNumber)).thenReturn(kit);
    return kit;
  }

  private ChainOfCustodyEvent mockCreateEvent(OrganizationUser actor, LocalDate eventDate) {
    ChainOfCustodyEvent createEvent = mock(ChainOfCustodyEvent.class);
    when(createEvent.getActor()).thenReturn(actor.getDisplayName());
    when(createEvent.getActorOrganization()).thenReturn(actor.getOrganization());
    when(createEvent.getTo()).thenReturn(actor.getOrganization());
    when(createEvent.getEventDate()).thenReturn(eventDate);
    when(createEvent.getEventType()).thenReturn(EventType.CREATE);
    return createEvent;
  }

  private ChainOfCustodyEvent mockSendEvent(OrganizationUser actor, Organization sendTo, LocalDate eventDate) {
    ChainOfCustodyEvent createEvent = mock(ChainOfCustodyEvent.class);
    when(createEvent.getActor()).thenReturn(actor.getDisplayName());
    when(createEvent.getActorOrganization()).thenReturn(actor.getOrganization());
    when(createEvent.getFrom()).thenReturn(actor.getOrganization());
    when(createEvent.getTo()).thenReturn(sendTo);
    when(createEvent.getEventDate()).thenReturn(eventDate);
    when(createEvent.getEventType()).thenReturn(EventType.SEND);
    return createEvent;
  }

  private ChainOfCustodyEvent mockReceiveEvent(OrganizationUser actor, Organization receivedFrom, LocalDate eventDate) {
    ChainOfCustodyEvent createEvent = mock(ChainOfCustodyEvent.class);
    when(createEvent.getActor()).thenReturn(actor.getDisplayName());
    when(createEvent.getActorOrganization()).thenReturn(actor.getOrganization());
    when(createEvent.getFrom()).thenReturn(receivedFrom);
    when(createEvent.getTo()).thenReturn(actor.getOrganization());
    when(createEvent.getEventDate()).thenReturn(eventDate);
    when(createEvent.getEventType()).thenReturn(EventType.SEND);
    return createEvent;
  }
}
