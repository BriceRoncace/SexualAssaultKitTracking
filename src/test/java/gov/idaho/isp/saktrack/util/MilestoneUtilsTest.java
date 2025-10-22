package gov.idaho.isp.saktrack.util;

import gov.idaho.isp.saktrack.config.EventBuilder;
import gov.idaho.isp.saktrack.domain.EventFlag;
import gov.idaho.isp.saktrack.domain.SexualAssaultKit;
import org.junit.Test;

import java.time.LocalDate;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;

public class MilestoneUtilsTest {

  @Test
  public void getTimeAtMedicalBasicTest() {
    SexualAssaultKit kit = new SexualAssaultKit();

    assertFalse(MilestoneUtils.getTimeAtMedical(kit).isPresent());

    kit.getMedicalDetails().setCollectionDate(LocalDate.now().minusDays(10L));

    assertEquals(Long.valueOf(10), MilestoneUtils.getTimeAtMedical(kit).get());

    kit.addChainOfCustodyEvent(new EventBuilder().eventFlag(EventFlag.MEDICAL_SENT_TO_LE).eventDate(LocalDate.now().minusDays(5L)).build());

    assertEquals(Long.valueOf(5), MilestoneUtils.getTimeAtMedical(kit).get());
  }

  @Test
  public void getTimeAtMedicalEarliestTest() {
    SexualAssaultKit kit = new SexualAssaultKit();

    assertFalse(MilestoneUtils.getTimeAtMedical(kit).isPresent());

    kit.getMedicalDetails().setCollectionDate(LocalDate.now().minusDays(10L));

    assertEquals(Long.valueOf(10), MilestoneUtils.getTimeAtMedical(kit).get());

    kit.addChainOfCustodyEvent(new EventBuilder().eventFlag(EventFlag.MEDICAL_SENT_TO_LE).eventDate(LocalDate.now().minusDays(5L)).build());
    kit.addChainOfCustodyEvent(new EventBuilder().eventFlag(EventFlag.MEDICAL_SENT_TO_LE).eventDate(LocalDate.now().minusDays(3L)).build());

    assertEquals(Long.valueOf(5), MilestoneUtils.getTimeAtMedical(kit).get());
  }

  @Test
  public void getTimeFromMedicalToLeBasicTest() {
    SexualAssaultKit kit = new SexualAssaultKit();

    assertFalse(MilestoneUtils.getTimeFromMedicalToLe(kit).isPresent());

    kit.addChainOfCustodyEvent(new EventBuilder().eventFlag(EventFlag.MEDICAL_SENT_TO_LE).eventDate(LocalDate.now().minusDays(10L)).build());

    assertEquals(Long.valueOf(10), MilestoneUtils.getTimeFromMedicalToLe(kit).get());

    kit.addChainOfCustodyEvent(new EventBuilder().eventFlag(EventFlag.LE_RECEIVED_COLLECTED_KIT).eventDate(LocalDate.now().minusDays(5L)).build());

    assertEquals(Long.valueOf(5), MilestoneUtils.getTimeFromMedicalToLe(kit).get());
  }

  @Test
  public void getTimeFromMedicalToLeLatestTest() {
    SexualAssaultKit kit = new SexualAssaultKit();

    assertFalse(MilestoneUtils.getTimeFromMedicalToLe(kit).isPresent());

    kit.addChainOfCustodyEvent(new EventBuilder().eventFlag(EventFlag.MEDICAL_SENT_TO_LE).eventDate(LocalDate.now().minusDays(10L)).build());

    assertEquals(Long.valueOf(10), MilestoneUtils.getTimeFromMedicalToLe(kit).get());

    kit.addChainOfCustodyEvent(new EventBuilder().eventFlag(EventFlag.LE_RECEIVED_COLLECTED_KIT).eventDate(LocalDate.now().minusDays(5L)).build());
    kit.addChainOfCustodyEvent(new EventBuilder().eventFlag(EventFlag.LE_RECEIVED_COLLECTED_KIT).eventDate(LocalDate.now().minusDays(7L)).build());

    assertEquals(Long.valueOf(5), MilestoneUtils.getTimeFromMedicalToLe(kit).get());
  }

  @Test
  public void getTimeAtLawEnforcementBasicTest() {
    SexualAssaultKit kit = new SexualAssaultKit();

    assertFalse(MilestoneUtils.getTimeAtLawEnforcement(kit).isPresent());

    kit.addChainOfCustodyEvent(new EventBuilder().eventFlag(EventFlag.LE_RECEIVED_COLLECTED_KIT).eventDate(LocalDate.now().minusDays(10L)).build());

    assertEquals(Long.valueOf(10), MilestoneUtils.getTimeAtLawEnforcement(kit).get());

    kit.addChainOfCustodyEvent(new EventBuilder().eventFlag(EventFlag.LE_SENT_FOR_ANALYSIS).eventDate(LocalDate.now().minusDays(5L)).build());

    assertEquals(Long.valueOf(5), MilestoneUtils.getTimeAtLawEnforcement(kit).get());
  }

  @Test
  public void getTimeAtLawEnforcementEarliestTest() {
    SexualAssaultKit kit = new SexualAssaultKit();

    assertFalse(MilestoneUtils.getTimeAtLawEnforcement(kit).isPresent());

    kit.addChainOfCustodyEvent(new EventBuilder().eventFlag(EventFlag.LE_RECEIVED_COLLECTED_KIT).eventDate(LocalDate.now().minusDays(10L)).build());

    assertEquals(Long.valueOf(10), MilestoneUtils.getTimeAtLawEnforcement(kit).get());

    kit.addChainOfCustodyEvent(new EventBuilder().eventFlag(EventFlag.LE_SENT_FOR_ANALYSIS).eventDate(LocalDate.now().minusDays(5L)).build());
    kit.addChainOfCustodyEvent(new EventBuilder().eventFlag(EventFlag.LE_SENT_FOR_ANALYSIS).eventDate(LocalDate.now().minusDays(3L)).build());

    assertEquals(Long.valueOf(5), MilestoneUtils.getTimeAtLawEnforcement(kit).get());
  }

  @Test
  public void getTimeAtLawEnforcementAttorneyAggreedTest() {
    SexualAssaultKit kit = new SexualAssaultKit();

    assertFalse(MilestoneUtils.getTimeAtLawEnforcement(kit).isPresent());

    kit.addChainOfCustodyEvent(new EventBuilder().eventFlag(EventFlag.LE_RECEIVED_COLLECTED_KIT).eventDate(LocalDate.now().minusDays(10L)).build());

    assertEquals(Long.valueOf(10), MilestoneUtils.getTimeAtLawEnforcement(kit).get());

    kit.getLeDetails().setMeetsSubmissionCriteria(Boolean.FALSE);
    kit.getLegalDetails().setProsecutorAgrees(Boolean.TRUE);
    kit.getLegalDetails().setReviewFinalized(LocalDate.now().minusDays(5L));

    assertEquals(Long.valueOf(5), MilestoneUtils.getTimeAtLawEnforcement(kit).get());
  }

  @Test
  public void getTimeAtLabBasicTest() {
    SexualAssaultKit kit = new SexualAssaultKit();

    assertFalse(MilestoneUtils.getTimeAtLab(kit).isPresent());

    kit.addChainOfCustodyEvent(new EventBuilder().eventFlag(EventFlag.LAB_RECEIVED).eventDate(LocalDate.now().minusDays(10L)).build());

    assertEquals(Long.valueOf(10), MilestoneUtils.getTimeAtLab(kit).get());

    kit.getLabDetails().setDateCompleted(LocalDate.now().minusDays(5L));

    assertEquals(Long.valueOf(5), MilestoneUtils.getTimeAtLab(kit).get());
  }
}