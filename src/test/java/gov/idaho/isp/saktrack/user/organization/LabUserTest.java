package gov.idaho.isp.saktrack.user.organization;

import gov.idaho.isp.saktrack.LabDetails;
import gov.idaho.isp.saktrack.SexualAssaultKit;
import gov.idaho.isp.saktrack.config.MockObjUtils;
import gov.idaho.isp.saktrack.dto.CreateKitEventDetails;
import gov.idaho.isp.saktrack.dto.EventDetails;
import gov.idaho.isp.saktrack.exception.IllegalTransferException;
import gov.idaho.isp.saktrack.jurisdiction.Jurisdiction;
import gov.idaho.isp.saktrack.organization.Organization;
import gov.idaho.isp.saktrack.organization.OrganizationType;
import java.util.Arrays;
import org.junit.Assert;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class LabUserTest {
  private final ArgumentCaptor<LabUser> labUserCaptor = ArgumentCaptor.forClass(LabUser.class);
  private final ArgumentCaptor<CreateKitEventDetails> createKitDetailsCaptor = ArgumentCaptor.forClass(CreateKitEventDetails.class);
  private final ArgumentCaptor<EventDetails> eventDetailsCaptor = ArgumentCaptor.forClass(EventDetails.class);
  private final ArgumentCaptor<OrganizationType> typeCaptor = ArgumentCaptor.forClass(OrganizationType.class);
  private final UserKitService userKitService = mock(UserKitService.class);
  private final Jurisdiction jurisdiction = MockObjUtils.createJurisdiction();
  private final Organization org = MockObjUtils.createOrganization(OrganizationType.LAB, jurisdiction);
  private final LabUser user = MockObjUtils.createOrganizationUser(org);

  private final SexualAssaultKit badLabDetailsKit = mock(SexualAssaultKit.class);
  private final LabDetails badLabDetails = mock(LabDetails.class);

  public LabUserTest() {
    user.setUserKitService(userKitService);
    when(badLabDetailsKit.getLabDetails()).thenReturn(badLabDetails);
  }

  @Test
  public void batchCreateTest() {
    CreateKitEventDetails details = MockObjUtils.createCreateKitEventDetails();

    user.batchCreate(details);

    verify(userKitService, times(1)).create(labUserCaptor.capture(), createKitDetailsCaptor.capture());
    Assert.assertEquals(user, labUserCaptor.getValue());
    Assert.assertEquals(details, createKitDetailsCaptor.getValue());
  }

  @Test
  public void receiveTest() {
    EventDetails details = MockObjUtils.createEventDetails();

    user.receive(details);

    verify(userKitService, times(1)).receive(labUserCaptor.capture(), eventDetailsCaptor.capture());
    Assert.assertEquals(user, labUserCaptor.getValue());
    Assert.assertEquals(details, eventDetailsCaptor.getValue());
  }

  @Test(expected = IllegalTransferException.class)
  public void receiveValidationTest() {
    EventDetails details = MockObjUtils.createEventDetails();
    details.setSerialNumberList(Arrays.asList("1", "2"));

    user.receive(details);
  }

  @Test
  public void batchSendToMedicalTest() {
    EventDetails details = MockObjUtils.createEventDetails();

    user.batchSendToMedical(details);

    verify(userKitService, times(1)).send(labUserCaptor.capture(), eventDetailsCaptor.capture(), typeCaptor.capture());
    Assert.assertEquals(user, labUserCaptor.getValue());
    Assert.assertEquals(details, eventDetailsCaptor.getValue());
    Assert.assertEquals(OrganizationType.MEDICAL, typeCaptor.getValue());
  }

  @Test
  public void batchSendToLabTest() {
    EventDetails details = MockObjUtils.createEventDetails();

    user.batchSendToLab(details);

    verify(userKitService, times(1)).send(labUserCaptor.capture(), eventDetailsCaptor.capture(), typeCaptor.capture());
    Assert.assertEquals(user, labUserCaptor.getValue());
    Assert.assertEquals(details, eventDetailsCaptor.getValue());
    Assert.assertEquals(OrganizationType.LAB, typeCaptor.getValue());
  }

  @Test
  public void batchSendToLawEnforcementTest() {
    EventDetails details = MockObjUtils.createEventDetails();

    user.batchSendToLawEnforcement(details);

    verify(userKitService, times(1)).send(labUserCaptor.capture(), eventDetailsCaptor.capture(), typeCaptor.capture());
    Assert.assertEquals(user, labUserCaptor.getValue());
    Assert.assertEquals(details, eventDetailsCaptor.getValue());
    Assert.assertEquals(OrganizationType.LAW_ENFORCEMENT, typeCaptor.getValue());
  }

  @Test
  public void returnToLawEnforcementTest() {
    EventDetails details = MockObjUtils.createEventDetails();

    user.returnToLawEnforcement(details);

    verify(userKitService, times(1)).send(labUserCaptor.capture(), eventDetailsCaptor.capture(), typeCaptor.capture());
    Assert.assertEquals(user, labUserCaptor.getValue());
    Assert.assertEquals(details, eventDetailsCaptor.getValue());
    Assert.assertEquals(OrganizationType.LAW_ENFORCEMENT, typeCaptor.getValue());
  }

  @Test(expected = IllegalTransferException.class)
  public void returnToLawEnforcementValidationTest() {
    EventDetails details = MockObjUtils.createEventDetails();
    details.setSerialNumberList(Arrays.asList("1", "2"));

    user.returnToLawEnforcement(details);
  }

  @Test
  public void repurposeTest() {
    EventDetails details = MockObjUtils.createEventDetails();

    user.repurpose(details);

    verify(userKitService, times(1)).repurpose(labUserCaptor.capture(), eventDetailsCaptor.capture());
    Assert.assertEquals(user, labUserCaptor.getValue());
    Assert.assertEquals(details, eventDetailsCaptor.getValue());
  }

  @Test(expected = IllegalTransferException.class)
  public void repurposeValidationTest() {
    EventDetails details = MockObjUtils.createEventDetails();
    details.setSerialNumberList(Arrays.asList("1", "2"));

    user.repurpose(details);
  }
}
