package gov.idaho.isp.saktrack.util;

import gov.idaho.isp.saktrack.ChainOfCustodyEvent;
import gov.idaho.isp.saktrack.EventFlag;
import gov.idaho.isp.saktrack.SexualAssaultKit;
import java.time.LocalDate;
import static java.time.temporal.ChronoUnit.DAYS;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

public class MilestoneUtils {

  public static List<LocalDate> getAllDatesForEventFlag(SexualAssaultKit kit, EventFlag flag) {
    return kit.getChainOfCustody().stream().filter(c -> flag.equals(c.getEventFlag())).map(c -> c.getEventDate()).collect(Collectors.toList());
  }

  //Milestone Event Dates
  public static Optional<LocalDate> getMedicalCollectedDate(SexualAssaultKit kit) {
    return kit.getMedicalDetails() != null ? Optional.ofNullable(kit.getMedicalDetails().getCollectionDate()) : Optional.empty();
  }

  public static Optional<LocalDate> getMedicalSubmitDate(SexualAssaultKit kit) {
    return getEarliestEventDate(kit, EventFlag.MEDICAL_SENT_TO_LE);
  }

  public static Optional<LocalDate> getLeReceiveDate(SexualAssaultKit kit) {
    return getLatestEventDate(kit, EventFlag.LE_RECEIVED_COLLECTED_KIT);
  }

  public static Optional<LocalDate> getReleasedForProsecutorReviewDate(SexualAssaultKit kit) {
    return kit.getLeDetails() != null ? Optional.ofNullable(kit.getLegalDetails().getReleasedForReview()) : Optional.empty();
  }

  public static Optional<LocalDate> getProsecutorHasAgreedDate(SexualAssaultKit kit) {
    return kit.getLeDetails() != null ? Optional.ofNullable(kit.getLegalDetails().getReviewFinalized()): Optional.empty();
  }

  public static Optional<LocalDate> getLeSubmitDate(SexualAssaultKit kit) {
    return getEarliestEventDate(kit, EventFlag.LE_SENT_FOR_ANALYSIS);
  }

  public static Optional<LocalDate> getProsecutorAgreedDateOrSentToLabDate(SexualAssaultKit kit) {
    if (Boolean.FALSE.equals(kit.getLeDetails().getMeetsSubmissionCriteria()) && Boolean.TRUE.equals(kit.getLegalDetails().getProsecutorAgrees())) {
      return Optional.ofNullable(getProsecutorHasAgreedDate(kit).orElse(null));
    }
    return Optional.ofNullable(getLeSubmitDate(kit).orElse(null));
  }

  public static Optional<LocalDate> getLabReceiveDate(SexualAssaultKit kit) {
    return getLatestEventDate(kit, EventFlag.LAB_RECEIVED);
  }

  public static Optional<LocalDate> getLabCompleteDate(SexualAssaultKit kit) {
    return kit.getLabDetails() != null  ? Optional.ofNullable(kit.getLabDetails().getDateCompleted()) : Optional.empty();
  }

  //Time between Milestones
  public static Optional<Long> getTimeAtMedical(SexualAssaultKit kit) {
    return getMedicalCollectedDate(kit).map(d -> DAYS.between(d, getMedicalSubmitDate(kit).orElse(LocalDate.now())));
  }

  public static Optional<Long> getTimeFromMedicalToLe(SexualAssaultKit kit) {
    return getMedicalSubmitDate(kit).map(d -> DAYS.between(d, getLeReceiveDate(kit).orElse(LocalDate.now())));
  }

  public static Optional<Long> getTimeAtLawEnforcement(SexualAssaultKit kit) {
    return getLeReceiveDate(kit).map(d -> DAYS.between(d, getProsecutorAgreedDateOrSentToLabDate(kit).orElse(LocalDate.now())));
  }

  public static Optional<Long> getTimeFromLawEnforcementToLab(SexualAssaultKit kit) {
    return getProsecutorAgreedDateOrSentToLabDate(kit).map(d -> DAYS.between(d, getLabReceiveDate(kit).orElse(LocalDate.now())));
  }

  public static Optional<Long> getTimeAtLab(SexualAssaultKit kit) {
    return getLabReceiveDate(kit).map(d -> DAYS.between(d, getLabCompleteDate(kit).orElse(LocalDate.now())));
  }

  //Helper methods
  private static Optional<LocalDate> getEarliestEventDate(SexualAssaultKit kit, EventFlag flag) {
    return kit.getChainOfCustody().stream().filter(e -> flag.equals(e.getEventFlag())).map(ChainOfCustodyEvent::getEventDate).min((d1, d2) -> d1.compareTo(d2));
  }

  private static Optional<LocalDate> getLatestEventDate(SexualAssaultKit kit, EventFlag flag) {
    return kit.getChainOfCustody().stream().filter(e -> flag == e.getEventFlag()).map(ChainOfCustodyEvent::getEventDate).max((d1, d2) -> d1.compareTo(d2));
  }
}
