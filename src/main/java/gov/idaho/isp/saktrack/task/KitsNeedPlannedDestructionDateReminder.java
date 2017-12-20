package gov.idaho.isp.saktrack.task;

import gov.idaho.isp.saktrack.SexualAssaultKit;
import gov.idaho.isp.saktrack.organization.Organization;
import gov.idaho.isp.saktrack.persistence.SexualAssaultKitRepository;
import gov.idaho.isp.saktrack.service.EmailService;
import java.time.LocalDate;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import org.springframework.stereotype.Service;

@Service(value = "kitsNeedPlannedDestructionDateReminder")
public class KitsNeedPlannedDestructionDateReminder implements Task {
  private final SexualAssaultKitRepository sexualAssaultKitRepository;
  private final EmailService emailService;

  public KitsNeedPlannedDestructionDateReminder(SexualAssaultKitRepository sexualAssaultKitRepository, EmailService emailService) {
    this.sexualAssaultKitRepository = sexualAssaultKitRepository;
    this.emailService = emailService;
  }

  @Override
  public void run() {
    LocalDate collectedAYearAgo = LocalDate.now().minusYears(1);
    List<SexualAssaultKit> kits = sexualAssaultKitRepository.findKitsNeedDestructionDateReminder(collectedAYearAgo);
    Map<Organization, List<SexualAssaultKit>> kitMap = kits.stream().filter(kit -> kit.getMedicalDetails().getRequestingLeAgency() != null).collect(Collectors.groupingBy(kit -> kit.getMedicalDetails().getRequestingLeAgency()));
    kitMap.entrySet().forEach(entry -> emailService.sendPlannedDestructionDateReminder(entry.getValue()));
  }
}
