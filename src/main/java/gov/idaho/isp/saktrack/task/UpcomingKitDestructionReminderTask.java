package gov.idaho.isp.saktrack.task;

import gov.idaho.isp.saktrack.domain.SexualAssaultKit;
import gov.idaho.isp.saktrack.domain.SexualAssaultKitRepository;
import gov.idaho.isp.saktrack.service.email.EmailService;
import java.time.LocalDate;
import java.util.List;
import org.springframework.stereotype.Service;

@Service(value = "upcomingKitDestructionReminder")
public class UpcomingKitDestructionReminderTask implements Task {
  private final SexualAssaultKitRepository sexualAssaultKitRepository;
  private final EmailService emailService;

  public UpcomingKitDestructionReminderTask(SexualAssaultKitRepository sexualAssaultKitRepository, EmailService emailService) {
    this.sexualAssaultKitRepository = sexualAssaultKitRepository;
    this.emailService = emailService;
  }

  @Override
  public void run() {
    LocalDate sixMonthsFromNow = LocalDate.now().plusMonths(6);
    List<SexualAssaultKit> kitsToBeDestroyed = sexualAssaultKitRepository.findByLeDetailsPlannedDestructionDate(sixMonthsFromNow);
    kitsToBeDestroyed.stream()
      .filter(kit -> Boolean.TRUE.equals(kit.getLeDetails().getPlannedDestructionNotificationRequested()))
      .forEach(kit -> emailService.sendUpcomingKitDestructionEmail(kit));
  }
}
