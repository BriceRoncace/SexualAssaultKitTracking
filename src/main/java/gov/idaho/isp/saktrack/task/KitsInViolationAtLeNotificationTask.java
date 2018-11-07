package gov.idaho.isp.saktrack.task;

import gov.idaho.isp.saktrack.domain.SexualAssaultKit;
import gov.idaho.isp.saktrack.domain.SexualAssaultKitRepository;
import gov.idaho.isp.saktrack.service.email.EmailService;
import java.time.LocalDate;
import java.time.Month;
import java.util.List;
import org.springframework.stereotype.Service;

@Service(value = "kitsInViolationAtLeNotification")
public class KitsInViolationAtLeNotificationTask implements Task {
  private final SexualAssaultKitRepository sexualAssaultKitRepository;
  private final EmailService emailService;

  public KitsInViolationAtLeNotificationTask(SexualAssaultKitRepository sexualAssaultKitRepository, EmailService emailService) {
    this.sexualAssaultKitRepository = sexualAssaultKitRepository;
    this.emailService = emailService;
  }

  @Override
  public void run() {
    List<SexualAssaultKit> deliquentKits = sexualAssaultKitRepository.findKitsAtLeInViolation(LocalDate.now().minusDays(30), LocalDate.of(2017, Month.JANUARY, 1));
    deliquentKits.forEach(kit -> emailService.sendKitPastSubmitEmail(kit));
  }
}
