package gov.idaho.isp.saktrack.task;

import gov.idaho.isp.saktrack.SexualAssaultKit;
import gov.idaho.isp.saktrack.persistence.SexualAssaultKitRepository;
import gov.idaho.isp.saktrack.service.EmailService;
import java.time.LocalDate;
import java.util.List;
import org.springframework.stereotype.Service;

@Service(value = "kitsNeedLeActionReminder")
public class KitsNeedLeActionReminderTask implements Task {
  private final SexualAssaultKitRepository sexualAssaultKitRepository;
  private final EmailService emailService;

  public KitsNeedLeActionReminderTask(SexualAssaultKitRepository sexualAssaultKitRepository, EmailService emailService) {
    this.sexualAssaultKitRepository = sexualAssaultKitRepository;
    this.emailService = emailService;
  }

  @Override
  public void run() {
    sendDelinquentWarnings(23);
    sendDelinquentWarnings(27);
  }

  private void sendDelinquentWarnings(Integer daysOld) {
    List<SexualAssaultKit> deliquentKits = sexualAssaultKitRepository.findKitsThatNeedLeAction(LocalDate.now().minusDays(daysOld));
    deliquentKits.forEach(kit -> emailService.sendKitsNeedLeActionReminderEmail(kit));
  }
}
