package gov.idaho.isp.saktrack.domain.user.organization;

import gov.idaho.isp.saktrack.config.MockObjUtils;
import gov.idaho.isp.saktrack.domain.KitStatus;
import gov.idaho.isp.saktrack.domain.LawEnforcementDetails;
import gov.idaho.isp.saktrack.domain.MedicalDetails;
import gov.idaho.isp.saktrack.domain.SexualAssaultKit;
import gov.idaho.isp.saktrack.domain.dto.EventDetails;
import gov.idaho.isp.saktrack.domain.jurisdiction.Jurisdiction;
import gov.idaho.isp.saktrack.domain.organization.Organization;
import gov.idaho.isp.saktrack.domain.organization.OrganizationType;
import gov.idaho.isp.saktrack.exception.IllegalTransferException;
import java.util.Arrays;
import org.junit.Assert;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class LawEnforcementUserTest {
  private final ArgumentCaptor<LawEnforcementUser> leUserCaptor = ArgumentCaptor.forClass(LawEnforcementUser.class);
  private final ArgumentCaptor<EventDetails> eventDetailsCaptor = ArgumentCaptor.forClass(EventDetails.class);
  private final ArgumentCaptor<OrganizationType> typeCaptor = ArgumentCaptor.forClass(OrganizationType.class);
  private final UserKitService userKitService = mock(UserKitService.class);
  private final Jurisdiction jurisdiction = MockObjUtils.createJurisdiction();
  private final Organization org = MockObjUtils.createOrganization(OrganizationType.LAW_ENFORCEMENT, jurisdiction);
  private final LawEnforcementUser user = MockObjUtils.createOrganizationUser(org);

  private final SexualAssaultKit badLeDetailsMyAgencyUnSendToBeAnalyzedKit = mock(SexualAssaultKit.class);
  private final MedicalDetails myAgencyMedicalDetails = mock(MedicalDetails.class);
  private final LawEnforcementDetails badLawEnforcementDetails = mock(LawEnforcementDetails.class);

  public LawEnforcementUserTest() {
    user.setUserKitService(userKitService);
    org.setId(1L);

    when(badLeDetailsMyAgencyUnSendToBeAnalyzedKit.getMedicalDetails()).thenReturn(myAgencyMedicalDetails);
    when(badLeDetailsMyAgencyUnSendToBeAnalyzedKit.getLeDetails()).thenReturn(badLawEnforcementDetails);
    when(badLeDetailsMyAgencyUnSendToBeAnalyzedKit.getStatus()).thenReturn(KitStatus.READY_TO_SEND_FOR_ANALYSIS);

    when(myAgencyMedicalDetails.getRequestingLeAgency()).thenReturn(org);
  }

  @Test
  public void receiveTest() {
    EventDetails details = MockObjUtils.createEventDetails();

    user.batchReceive(details);

    verify(userKitService, times(1)).receive(leUserCaptor.capture(), eventDetailsCaptor.capture());
    Assert.assertEquals(user, leUserCaptor.getValue());
    Assert.assertEquals(details, eventDetailsCaptor.getValue());
  }

  @Test
  public void sendToLawEnforcementTest() {
    EventDetails details = MockObjUtils.createEventDetails();

    user.batchSendToLawEnforcement(details);

    verify(userKitService, times(1)).send(leUserCaptor.capture(), eventDetailsCaptor.capture(), typeCaptor.capture());
    Assert.assertEquals(user, leUserCaptor.getValue());
    Assert.assertEquals(details, eventDetailsCaptor.getValue());
    Assert.assertEquals(OrganizationType.LAW_ENFORCEMENT, typeCaptor.getValue());
  }

  @Test
  public void sendToLabTest() {
    EventDetails details = MockObjUtils.createEventDetails();

    user.batchSendToLab(details);

    verify(userKitService, times(1)).send(leUserCaptor.capture(), eventDetailsCaptor.capture(), typeCaptor.capture());
    Assert.assertEquals(user, leUserCaptor.getValue());
    Assert.assertEquals(details, eventDetailsCaptor.getValue());
    Assert.assertEquals(OrganizationType.LAB, typeCaptor.getValue());
  }

  @Test
  public void sendToMedicalTest() {
    EventDetails details = MockObjUtils.createEventDetails();

    user.batchSendToMedical(details);

    verify(userKitService, times(1)).send(leUserCaptor.capture(), eventDetailsCaptor.capture(), typeCaptor.capture());
    Assert.assertEquals(user, leUserCaptor.getValue());
    Assert.assertEquals(details, eventDetailsCaptor.getValue());
    Assert.assertEquals(OrganizationType.MEDICAL, typeCaptor.getValue());
  }

  @Test
  public void destroyTest() {
    EventDetails details = MockObjUtils.createEventDetails();

    user.destroy(details);

    verify(userKitService, times(1)).destroy(leUserCaptor.capture(), eventDetailsCaptor.capture());
    Assert.assertEquals(user, leUserCaptor.getValue());
    Assert.assertEquals(details, eventDetailsCaptor.getValue());
  }

  @Test(expected = IllegalTransferException.class)
  public void destroyValidationTest() {
    EventDetails details = MockObjUtils.createEventDetails();
    details.setSerialNumberList(Arrays.asList("1", "2"));

    user.destroy(details);
  }
}