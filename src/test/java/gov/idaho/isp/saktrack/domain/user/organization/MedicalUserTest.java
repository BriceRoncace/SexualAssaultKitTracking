package gov.idaho.isp.saktrack.domain.user.organization;

import gov.idaho.isp.saktrack.config.MockObjUtils;
import gov.idaho.isp.saktrack.domain.KitStatus;
import gov.idaho.isp.saktrack.domain.MedicalDetails;
import gov.idaho.isp.saktrack.domain.SexualAssaultKit;
import gov.idaho.isp.saktrack.domain.dto.EventDetails;
import gov.idaho.isp.saktrack.domain.jurisdiction.Jurisdiction;
import gov.idaho.isp.saktrack.domain.organization.Organization;
import gov.idaho.isp.saktrack.domain.organization.OrganizationType;
import gov.idaho.isp.saktrack.exception.IllegalTransferException;
import java.time.LocalDate;
import java.util.Arrays;
import org.junit.Assert;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class MedicalUserTest {
  private final ArgumentCaptor<MedicalUser> medicalUserCaptor = ArgumentCaptor.forClass(MedicalUser.class);
  private final ArgumentCaptor<EventDetails> eventDetailsCaptor = ArgumentCaptor.forClass(EventDetails.class);
  private final ArgumentCaptor<OrganizationType> typeCaptor = ArgumentCaptor.forClass(OrganizationType.class);
  private final UserKitService userKitService = mock(UserKitService.class);
  private final Jurisdiction jurisdiction = MockObjUtils.createJurisdiction();
  private final Organization org = MockObjUtils.createOrganization(OrganizationType.MEDICAL, jurisdiction);
  private final MedicalUser user = MockObjUtils.createOrganizationUser(org);

  private final SexualAssaultKit badCollectedButUnreceivedKit = mock(SexualAssaultKit.class);
  private final MedicalDetails badMedicalDetails = mock(MedicalDetails.class);

  public MedicalUserTest() {
    user.setUserKitService(userKitService);

    when(badCollectedButUnreceivedKit.getStatus()).thenReturn(KitStatus.COLLECTED_BUT_UNRECEIVED);
    when(badCollectedButUnreceivedKit.getMedicalDetails()).thenReturn(badMedicalDetails);
    when(badMedicalDetails.getCollectionDate()).thenReturn(LocalDate.now());
  }

  @Test
  public void batchReceiveTest() {
    EventDetails details = MockObjUtils.createEventDetails();

    user.batchReceive(details);

    verify(userKitService, times(1)).receive(medicalUserCaptor.capture(), eventDetailsCaptor.capture());
    Assert.assertEquals(user, medicalUserCaptor.getValue());
    Assert.assertEquals(details, eventDetailsCaptor.getValue());
  }

  @Test
  public void sendToLawEnforcementTest() {
    EventDetails details = MockObjUtils.createEventDetails();

    user.batchSendToLawEnforcement(details);

    verify(userKitService, times(1)).send(medicalUserCaptor.capture(), eventDetailsCaptor.capture(), typeCaptor.capture());
    Assert.assertEquals(user, medicalUserCaptor.getValue());
    Assert.assertEquals(details, eventDetailsCaptor.getValue());
    Assert.assertEquals(OrganizationType.LAW_ENFORCEMENT, typeCaptor.getValue());
  }

  @Test
  public void batchSendToMedicalTest() {
    EventDetails details = MockObjUtils.createEventDetails();

    user.batchSendToMedical(details);

    verify(userKitService, times(1)).send(medicalUserCaptor.capture(), eventDetailsCaptor.capture(), typeCaptor.capture());
    Assert.assertEquals(user, medicalUserCaptor.getValue());
    Assert.assertEquals(details, eventDetailsCaptor.getValue());
    Assert.assertEquals(OrganizationType.MEDICAL, typeCaptor.getValue());
  }

  @Test
  public void repurposeTest() {
    EventDetails details = MockObjUtils.createEventDetails();

    user.repurpose(details);

    verify(userKitService, times(1)).repurpose(medicalUserCaptor.capture(), eventDetailsCaptor.capture());
    Assert.assertEquals(user, medicalUserCaptor.getValue());
    Assert.assertEquals(details, eventDetailsCaptor.getValue());
  }

  @Test(expected = IllegalTransferException.class)
  public void repurposeValidationTest() {
    EventDetails details = MockObjUtils.createEventDetails();
    details.setSerialNumberList(Arrays.asList("1", "2"));

    user.repurpose(details);
  }
}
