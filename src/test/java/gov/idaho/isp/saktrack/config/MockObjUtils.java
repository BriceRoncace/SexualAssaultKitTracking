package gov.idaho.isp.saktrack.config;

import gov.idaho.isp.saktrack.ChainOfCustodyEvent;
import gov.idaho.isp.saktrack.ChainOfCustodyEvent.EventType;
import gov.idaho.isp.saktrack.LabDetails;
import gov.idaho.isp.saktrack.LawEnforcementDetails;
import gov.idaho.isp.saktrack.LegalDetails;
import gov.idaho.isp.saktrack.MedicalDetails;
import gov.idaho.isp.saktrack.SexualAssaultKit;
import gov.idaho.isp.saktrack.YesNoNa;
import gov.idaho.isp.saktrack.dto.CreateKitEventDetails;
import gov.idaho.isp.saktrack.dto.EventDetails;
import gov.idaho.isp.saktrack.jurisdiction.Jurisdiction;
import gov.idaho.isp.saktrack.organization.Organization;
import gov.idaho.isp.saktrack.organization.OrganizationType;
import gov.idaho.isp.saktrack.user.AdminUser;
import gov.idaho.isp.saktrack.user.User;
import gov.idaho.isp.saktrack.user.organization.AbstractOrganizationUser;
import gov.idaho.isp.saktrack.user.organization.OrganizationUser;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;

public class MockObjUtils {
  private static final List<Integer> INT_USED = new ArrayList<>();

  public static Organization createOrganization(OrganizationType type, Jurisdiction jurisdiction) {
    Integer id = getUniqueId();
    Organization org = new Organization();
    org.setName(type.getLabel() + id);
    org.setPasskey("P@$Sk3y" + id);
    org.setType(type);
    org.setJurisdiction(jurisdiction);
    return org;
  }

  public static Jurisdiction createJurisdiction() {
    Integer id = getUniqueId();
    Jurisdiction jurisdiction = new Jurisdiction();
    jurisdiction.setName("name" + id);
    jurisdiction.setType(Jurisdiction.Type.COUNTY);
    return jurisdiction;
  }

  public static AdminUser createAdminUser() {
    Integer id = getUniqueId();
    AdminUser user = new AdminUser();
    user.setUsername("username" + id);
    user.setDisplayName("displayName" + id);
    return user;
  }

  public static <T extends OrganizationUser> T createOrganizationUser(Organization org) {
    Integer id = getUniqueId();
    AbstractOrganizationUser user = org.getType().getNewUser();
    user.setUsername("username" + id);
    user.setDisplayName(org.getType().getLabel() + "User" + id);
    user.setOrganization(org);
    user.setPasskey(org.getPasskey());
    user.setPhone("1-208-123-4567");
    user.setEmail(user.getUsername() + "@bogus-email.com");
    user.setOrganizationAdmin(Boolean.FALSE);
    user.setAuthMethod(User.AuthMethod.DATABASE);
    return (T) user;
  }

  public static EventDetails createEventDetails() {
    Integer id = getUniqueId();
    EventDetails eventDetails = new EventDetails();
    eventDetails.setEventDate(LocalDate.now());
    eventDetails.setNotes("notes" + id);
    return eventDetails;
  }

  public static CreateKitEventDetails createCreateKitEventDetails() {
    Integer id = getUniqueId();
    CreateKitEventDetails eventDetails = new CreateKitEventDetails();
    eventDetails.setEventDate(LocalDate.now());
    eventDetails.setNotes("notes" + id);
    eventDetails.setExpirationDate(LocalDate.now().plusYears(1L));
    return eventDetails;
  }

  public static ChainOfCustodyEvent createChainOfCustodyEvent(EventType type, OrganizationUser user, Organization from, Organization to) {
    Integer id = getUniqueId();
    ChainOfCustodyEvent event = new ChainOfCustodyEvent();
    event.setEventType(type);
    event.setActor(user.getDisplayName());
    event.setActorOrganization(user.getOrganization());
    event.setFrom(from);
    event.setTo(to);
    event.setEventDate(LocalDate.now());
    event.setDigitalTimestamp(LocalDateTime.now());
    event.setNotes("notes" + id);
    return event;
  }

  public static SexualAssaultKit createSexualAssaultKit(Organization org) {
    Integer id = getUniqueId();
    SexualAssaultKit kit = new SexualAssaultKit();
    kit.setSerialNumber(id.toString());
    kit.setExpirationDate(LocalDate.now().plusYears(1));
    kit.setCurrentAssignment(org);
    kit.setMedicalDetails(getMedicalDetails(org));
    kit.setLeDetails(getLawEnforcementDetails(id));
    kit.setLegalDetails(getLegalDetails(id));
    kit.setLabDetails(getLabDetails(id, org));
    kit.setQuestionableEvents(false);
    return kit;
  }

  public static LabDetails getLabDetails(Integer id, Organization requestingAgency) {
    LabDetails ld = new LabDetails();
    ld.setRequestingLeAgency(requestingAgency);
    ld.setCaseNumber("case number" + id);
    ld.setDateCompleted(LocalDate.now());
    ld.setDnaDatabaseEntry(YesNoNa.YES);
    ld.setDnaDatabaseHitDate(LocalDate.now().minusDays(1L));
    ld.setExpungedDate(LocalDate.now().minusDays(1L));
    return ld;
  }

  public static LawEnforcementDetails getLawEnforcementDetails(Integer id) {
    LawEnforcementDetails led = new LawEnforcementDetails();
    led.setCaseNumber("case number" + id);
    led.setInvestigator("investigator" + id);
    led.setCrime("crime" + id);
    led.setCrimeDate(LocalDate.now().minusMonths(1L));
    led.setOutsourcedLabName("lab name" + id);
    led.setPlannedDestructionDate(LocalDate.now().plusMonths(1L));
    led.setMeetsSubmissionCriteria(Boolean.TRUE);
    led.setNonSubmissionReason(LawEnforcementDetails.NonSubmissionReason.NOT_A_CRIME);
    return led;
  }

  public static LegalDetails getLegalDetails(Integer id) {
    LegalDetails ld = new LegalDetails();
    ld.setProsecutorNotes("prosecutor notes" + id);
    ld.setProsecutorAgrees(Boolean.TRUE);
    ld.setReviewingProsecutor("district attorney" + id);
    return ld;
  }

  public static MedicalDetails getMedicalDetails(Organization submittingAgency) {
    MedicalDetails md = new MedicalDetails();
    md.setRequestingLeAgency(submittingAgency);
    md.setVictimType(MedicalDetails.VictimType.NAMED);
    md.setCollectionDate(LocalDate.now());
    return md;
  }

  public static Object deepCopy(Object orig) {
    Object obj = null;
    try {
      // Write the object out to a byte array
      ByteArrayOutputStream bos = new ByteArrayOutputStream();
      ObjectOutputStream out = new ObjectOutputStream(bos);
      out.writeObject(orig);
      out.flush();
      out.close();

      // Make an input stream from the byte array and read
      // a copy of the object back in.
      ObjectInputStream in = new ObjectInputStream(
        new ByteArrayInputStream(bos.toByteArray()));
      obj = in.readObject();
    }
    catch (IOException | ClassNotFoundException e) {
      e.printStackTrace();
    }
    return obj;
  }

  public static Integer getUniqueId() {
    Random rand = new Random();
    Integer randomInt = null;

    do {
      randomInt = rand.nextInt(2147483646);
      if (INT_USED.contains(randomInt)) {
        randomInt = null;
      }
      else {
        INT_USED.add(randomInt);
      }
    }
    while (randomInt == null);

    return randomInt;
  }
}
