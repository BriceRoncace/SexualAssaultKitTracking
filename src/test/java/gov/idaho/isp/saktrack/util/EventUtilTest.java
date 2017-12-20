package gov.idaho.isp.saktrack.util;

import gov.idaho.isp.saktrack.ChainOfCustodyEvent;
import gov.idaho.isp.saktrack.SexualAssaultKit;
import gov.idaho.isp.saktrack.config.MockObjUtils;
import gov.idaho.isp.saktrack.dto.EventDetails;
import gov.idaho.isp.saktrack.jurisdiction.Jurisdiction;
import gov.idaho.isp.saktrack.organization.Organization;
import gov.idaho.isp.saktrack.organization.OrganizationType;
import gov.idaho.isp.saktrack.user.organization.LabUser;
import gov.idaho.isp.saktrack.user.organization.LawEnforcementUser;
import gov.idaho.isp.saktrack.user.organization.MedicalUser;
import gov.idaho.isp.saktrack.user.organization.OrganizationUser;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.Month;
import org.junit.Assert;
import org.junit.Test;

public class EventUtilTest {

  public Jurisdiction jurisdiction = MockObjUtils.createJurisdiction();

  public Organization lab = MockObjUtils.createOrganization(OrganizationType.LAB, jurisdiction);
  public LabUser labUser = MockObjUtils.createOrganizationUser(lab);

  public Organization medical = MockObjUtils.createOrganization(OrganizationType.MEDICAL, jurisdiction);
  public MedicalUser medicalUser = MockObjUtils.createOrganizationUser(medical);

  public Organization le = MockObjUtils.createOrganization(OrganizationType.LAW_ENFORCEMENT, jurisdiction);
  public LawEnforcementUser leUser = MockObjUtils.createOrganizationUser(le);

  @Test
  public void validSendAndReceiveEventDatesInTheStandardKitLifeCycle() {
    SexualAssaultKit kit = new SexualAssaultKit();
    kit.setSerialNumber("123");

    EventDetails createDetails = new EventDetails();
    createDetails.setEventDate(LocalDate.of(2017, Month.JANUARY, 1));
    kit.addChainOfCustodyEvent(createEvent(labUser, ChainOfCustodyEvent.EventType.CREATE, createDetails, null, labUser.getOrganization()));

    EventDetails labToMedDetails = new EventDetails();
    labToMedDetails.setEventDate(LocalDate.of(2017, Month.JANUARY, 2));
    Assert.assertTrue(EventUtil.validSendDate(kit, labUser.getOrganization(), medicalUser.getOrganization(), labToMedDetails));
    addSendEvent(kit, labUser, medicalUser.getOrganization(), labToMedDetails);

    EventDetails medFromLabDetails = new EventDetails();
    medFromLabDetails.setEventDate(LocalDate.of(2017, Month.JANUARY, 3));
    Assert.assertTrue(EventUtil.validReceiveDate(kit, medicalUser.getOrganization(), labUser.getOrganization(), medFromLabDetails));
    addReceiveEvent(kit, medicalUser, labUser.getOrganization(), medFromLabDetails);

    EventDetails medToLeDetails = new EventDetails();
    medToLeDetails.setEventDate(LocalDate.of(2017, Month.JANUARY, 4));
    Assert.assertTrue(EventUtil.validSendDate(kit, medicalUser.getOrganization(), leUser.getOrganization(), medToLeDetails));
    addSendEvent(kit, medicalUser, leUser.getOrganization(), medToLeDetails);

    EventDetails leFromMedDetails = new EventDetails();
    leFromMedDetails.setEventDate(LocalDate.of(2017, Month.JANUARY, 5));
    Assert.assertTrue(EventUtil.validReceiveDate(kit, leUser.getOrganization(), medicalUser.getOrganization(), leFromMedDetails));
    addReceiveEvent(kit, leUser, medicalUser.getOrganization(), leFromMedDetails);

    EventDetails leToLabDetails = new EventDetails();
    leToLabDetails.setEventDate(LocalDate.of(2017, Month.JANUARY, 6));
    Assert.assertTrue(EventUtil.validSendDate(kit, leUser.getOrganization(), labUser.getOrganization(), leToLabDetails));
    addSendEvent(kit, leUser, labUser.getOrganization(), leToLabDetails);

    EventDetails labFromLeDetails = new EventDetails();
    labFromLeDetails.setEventDate(LocalDate.of(2017, Month.JANUARY, 7));
    Assert.assertTrue(EventUtil.validReceiveDate(kit, labUser.getOrganization(), leUser.getOrganization(), labFromLeDetails));
    addReceiveEvent(kit, labUser, leUser.getOrganization(), labFromLeDetails);

    EventDetails labToLeDetails = new EventDetails();
    labToLeDetails.setEventDate(LocalDate.of(2017, Month.JANUARY, 8));
    Assert.assertTrue(EventUtil.validSendDate(kit, labUser.getOrganization(), leUser.getOrganization(), labToLeDetails));
    addSendEvent(kit, labUser, leUser.getOrganization(), labToLeDetails);

    EventDetails leFromLabDetails = new EventDetails();
    leFromLabDetails.setEventDate(LocalDate.of(2017, Month.JANUARY, 9));
    Assert.assertTrue(EventUtil.validReceiveDate(kit, leUser.getOrganization(), labUser.getOrganization(), leFromLabDetails));
    addReceiveEvent(kit, leUser, labUser.getOrganization(), leFromLabDetails);
  }

  @Test
  public void validSendingTheKitToLabFromLeTwice() {
    SexualAssaultKit kit = new SexualAssaultKit();
    kit.setSerialNumber("123");

    EventDetails createDetails = new EventDetails();
    createDetails.setEventDate(LocalDate.of(2017, Month.JANUARY, 1));
    kit.addChainOfCustodyEvent(createEvent(labUser, ChainOfCustodyEvent.EventType.CREATE, createDetails, null, labUser.getOrganization()));

    EventDetails leToLabDetails = new EventDetails();
    leToLabDetails.setEventDate(LocalDate.of(2017, Month.JANUARY, 6));
    Assert.assertTrue(EventUtil.validSendDate(kit, leUser.getOrganization(), labUser.getOrganization(), leToLabDetails));
    addSendEvent(kit, leUser, labUser.getOrganization(), leToLabDetails);

    EventDetails labFromLeDetails = new EventDetails();
    labFromLeDetails.setEventDate(LocalDate.of(2017, Month.JANUARY, 7));
    Assert.assertTrue(EventUtil.validReceiveDate(kit, labUser.getOrganization(), leUser.getOrganization(), labFromLeDetails));
    addReceiveEvent(kit, labUser, leUser.getOrganization(), labFromLeDetails);

    EventDetails labToLeDetails = new EventDetails();
    labToLeDetails.setEventDate(LocalDate.of(2017, Month.JANUARY, 8));
    Assert.assertTrue(EventUtil.validSendDate(kit, labUser.getOrganization(), leUser.getOrganization(), labToLeDetails));
    addSendEvent(kit, labUser, leUser.getOrganization(), labToLeDetails);

    EventDetails leFromLabDetails = new EventDetails();
    leFromLabDetails.setEventDate(LocalDate.of(2017, Month.JANUARY, 9));
    Assert.assertTrue(EventUtil.validReceiveDate(kit, leUser.getOrganization(), labUser.getOrganization(), leFromLabDetails));
    addReceiveEvent(kit, leUser, labUser.getOrganization(), leFromLabDetails);

    EventDetails leToLabAgainDetails = new EventDetails();
    leToLabAgainDetails.setEventDate(LocalDate.of(2017, Month.JANUARY, 10));
    Assert.assertTrue(EventUtil.validSendDate(kit, leUser.getOrganization(), labUser.getOrganization(), leToLabAgainDetails));
    addSendEvent(kit, leUser, labUser.getOrganization(), leToLabAgainDetails);

    EventDetails labFromLeAgainDetails = new EventDetails();
    labFromLeAgainDetails.setEventDate(LocalDate.of(2017, Month.JANUARY, 11));
    Assert.assertTrue(EventUtil.validReceiveDate(kit, labUser.getOrganization(), leUser.getOrganization(), labFromLeAgainDetails));
    addReceiveEvent(kit, labUser, leUser.getOrganization(), labFromLeAgainDetails);
  }

  @Test
  public void validSameDaySendAndReceive() {
    SexualAssaultKit kit = new SexualAssaultKit();
    kit.setSerialNumber("123");

    EventDetails everythingDoneTheSameDayDetails = new EventDetails();
    everythingDoneTheSameDayDetails.setEventDate(LocalDate.of(2017, Month.JANUARY, 1));

    kit.addChainOfCustodyEvent(createEvent(labUser, ChainOfCustodyEvent.EventType.CREATE, everythingDoneTheSameDayDetails, null, labUser.getOrganization()));

    Assert.assertTrue(EventUtil.validSendDate(kit, labUser.getOrganization(), medicalUser.getOrganization(), everythingDoneTheSameDayDetails));
    addSendEvent(kit, labUser, medicalUser.getOrganization(), everythingDoneTheSameDayDetails);

    Assert.assertTrue(EventUtil.validReceiveDate(kit, medicalUser.getOrganization(), labUser.getOrganization(), everythingDoneTheSameDayDetails));
    addReceiveEvent(kit, medicalUser, labUser.getOrganization(), everythingDoneTheSameDayDetails);

    Assert.assertTrue(EventUtil.validSendDate(kit, medicalUser.getOrganization(), leUser.getOrganization(), everythingDoneTheSameDayDetails));
    addSendEvent(kit, medicalUser, leUser.getOrganization(), everythingDoneTheSameDayDetails);

    Assert.assertTrue(EventUtil.validReceiveDate(kit, leUser.getOrganization(), medicalUser.getOrganization(), everythingDoneTheSameDayDetails));
    addReceiveEvent(kit, leUser, medicalUser.getOrganization(), everythingDoneTheSameDayDetails);

    Assert.assertTrue(EventUtil.validSendDate(kit, leUser.getOrganization(), labUser.getOrganization(), everythingDoneTheSameDayDetails));
    addSendEvent(kit, leUser, labUser.getOrganization(), everythingDoneTheSameDayDetails);

    Assert.assertTrue(EventUtil.validReceiveDate(kit, labUser.getOrganization(), leUser.getOrganization(), everythingDoneTheSameDayDetails));
    addReceiveEvent(kit, labUser, leUser.getOrganization(), everythingDoneTheSameDayDetails);

    Assert.assertTrue(EventUtil.validSendDate(kit, labUser.getOrganization(), leUser.getOrganization(), everythingDoneTheSameDayDetails));
    addSendEvent(kit, labUser, leUser.getOrganization(), everythingDoneTheSameDayDetails);

    Assert.assertTrue(EventUtil.validReceiveDate(kit, leUser.getOrganization(), labUser.getOrganization(), everythingDoneTheSameDayDetails));
    addReceiveEvent(kit, leUser, labUser.getOrganization(), everythingDoneTheSameDayDetails);
  }

  @Test
  public void invalidSendDateBeforeCreation() {
    SexualAssaultKit kit = new SexualAssaultKit();
    kit.setSerialNumber("123");

    EventDetails createDetails = new EventDetails();
    createDetails.setEventDate(LocalDate.of(2017, Month.JANUARY, 1));
    kit.addChainOfCustodyEvent(createEvent(labUser, ChainOfCustodyEvent.EventType.CREATE, createDetails, null, labUser.getOrganization()));

    EventDetails labToMedDetails = new EventDetails();
    labToMedDetails.setEventDate(LocalDate.of(2016, Month.DECEMBER, 31));
    Assert.assertFalse(EventUtil.validSendDate(kit, labUser.getOrganization(), medicalUser.getOrganization(), labToMedDetails));
  }

  @Test
  public void invalidReceiveDateBeforeCreation() {
    SexualAssaultKit kit = new SexualAssaultKit();
    kit.setSerialNumber("123");

    EventDetails createDetails = new EventDetails();
    createDetails.setEventDate(LocalDate.of(2017, Month.JANUARY, 1));
    kit.addChainOfCustodyEvent(createEvent(labUser, ChainOfCustodyEvent.EventType.CREATE, createDetails, null, labUser.getOrganization()));

    EventDetails medFromLabDetails = new EventDetails();
    medFromLabDetails.setEventDate(LocalDate.of(2016, Month.DECEMBER, 31));
    Assert.assertFalse(EventUtil.validReceiveDate(kit, medicalUser.getOrganization(), labUser.getOrganization(), medFromLabDetails));
  }

  @Test
  public void sendDateBeforeReceivedDateShouldBeInvalid() {
    SexualAssaultKit kit = new SexualAssaultKit();
    kit.setSerialNumber("123");

    // kit created
    EventDetails createDetails = new EventDetails();
    createDetails.setEventDate(LocalDate.of(2017, Month.JANUARY, 1));
    kit.addChainOfCustodyEvent(createEvent(labUser, ChainOfCustodyEvent.EventType.CREATE, createDetails, null, labUser.getOrganization()));

    // kit sent to medical
    EventDetails labToMedSend = new EventDetails();
    labToMedSend.setEventDate(LocalDate.of(2017, Month.JANUARY, 2));
    Assert.assertTrue(EventUtil.validSendDate(kit, labUser.getOrganization(), medicalUser.getOrganization(), labToMedSend));
    addSendEvent(kit, labUser, medicalUser.getOrganization(), labToMedSend);

    // kit received from lab by medical
    EventDetails labToMedReceive = new EventDetails();
    labToMedReceive.setEventDate(LocalDate.of(2017, Month.JANUARY, 3));
    Assert.assertTrue(EventUtil.validReceiveDate(kit, medicalUser.getOrganization(), labUser.getOrganization(), labToMedReceive));
    addReceiveEvent(kit, medicalUser, labUser.getOrganization(), labToMedReceive);

    // kit sent by medical to LE but on a date prior to when the kit was received
    EventDetails medToLeReceive = new EventDetails();
    medToLeReceive.setEventDate(LocalDate.of(2017, Month.JANUARY, 1));
    Assert.assertFalse(EventUtil.validSendDate(kit, medicalUser.getOrganization(), leUser.getOrganization(), medToLeReceive));
  }

  @Test
  public void invalidSendDateAfterActuallyReceived() {
    SexualAssaultKit kit = new SexualAssaultKit();
    kit.setSerialNumber("123");

    EventDetails createDetails = new EventDetails();
    createDetails.setEventDate(LocalDate.of(2017, Month.JANUARY, 1));
    kit.addChainOfCustodyEvent(createEvent(labUser, ChainOfCustodyEvent.EventType.CREATE, createDetails, null, labUser.getOrganization()));

    //Medical User takes possession of a kit before it is sent to them through the API
    EventDetails medFromLabDetails = new EventDetails();
    medFromLabDetails.setEventDate(LocalDate.of(2017, Month.JANUARY, 3));
    Assert.assertTrue(EventUtil.validReceiveDate(kit, medicalUser.getOrganization(), labUser.getOrganization(), medFromLabDetails));
    addReceiveEvent(kit, medicalUser, labUser.getOrganization(), medFromLabDetails);

    //Post-sending, Lab tries to record they sent the kit
    //The kit must be sent to medical before it can be received by medical
    EventDetails labToMedDetails = new EventDetails();
    labToMedDetails.setEventDate(LocalDate.of(2017, Month.JANUARY, 4)); //After the medical receive date
    Assert.assertFalse(EventUtil.validSendDate(kit, labUser.getOrganization(), medicalUser.getOrganization(), labToMedDetails));
  }

  @Test
  public void invalidReceiveDateBeforeSend() {
    SexualAssaultKit kit = new SexualAssaultKit();
    kit.setSerialNumber("123");

    EventDetails createDetails = new EventDetails();
    createDetails.setEventDate(LocalDate.of(2017, Month.JANUARY, 1));
    kit.addChainOfCustodyEvent(createEvent(labUser, ChainOfCustodyEvent.EventType.CREATE, createDetails, null, labUser.getOrganization()));

    EventDetails labToMedDetails = new EventDetails();
    labToMedDetails.setEventDate(LocalDate.of(2017, Month.JANUARY, 2));
    Assert.assertTrue(EventUtil.validSendDate(kit, labUser.getOrganization(), medicalUser.getOrganization(), labToMedDetails));
    addSendEvent(kit, labUser, medicalUser.getOrganization(), labToMedDetails);

    EventDetails medFromLabDetails = new EventDetails();
    medFromLabDetails.setEventDate(LocalDate.of(2017, Month.JANUARY, 1));
    Assert.assertFalse(EventUtil.validReceiveDate(kit, medicalUser.getOrganization(), labUser.getOrganization(), medFromLabDetails));
  }

  private void addSendEvent(SexualAssaultKit kit, OrganizationUser actor, Organization to, EventDetails eventDetails) {
    kit.addChainOfCustodyEvent(createEvent(actor, ChainOfCustodyEvent.EventType.SEND, eventDetails, actor.getOrganization(), to));
  }

  private void addReceiveEvent(SexualAssaultKit kit, OrganizationUser actor, Organization from, EventDetails eventDetails) {
    kit.addChainOfCustodyEvent(createEvent(actor, ChainOfCustodyEvent.EventType.RECEIVE, eventDetails, from, actor.getOrganization()));
  }

  private ChainOfCustodyEvent createEvent(OrganizationUser actor, ChainOfCustodyEvent.EventType eventType, EventDetails eventDetails, Organization from, Organization to) {
    ChainOfCustodyEvent event = new ChainOfCustodyEvent();
    event.setEventType(eventType);
    event.setEventDate(eventDetails.getEventDate());
    event.setDigitalTimestamp(LocalDateTime.now());
    event.setActor(actor.getDisplayName());
    event.setActorOrganization(actor.getOrganization());
    event.setFrom(from);
    event.setTo(to);
    event.setEventFlag(null);
    return event;
  }

}
