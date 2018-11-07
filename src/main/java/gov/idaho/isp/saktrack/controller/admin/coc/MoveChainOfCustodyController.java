package gov.idaho.isp.saktrack.controller.admin.coc;

import gov.idaho.isp.saktrack.domain.ChainOfCustodyEvent;
import gov.idaho.isp.saktrack.domain.SexualAssaultKit;
import gov.idaho.isp.saktrack.domain.SexualAssaultKitRepository;
import gov.idaho.isp.saktrack.domain.user.User;
import gov.idaho.isp.saktrack.service.AuditService;
import gov.idaho.isp.saktrack.util.CollectionMoveUtil;
import gov.idaho.isp.saktrack.util.CollectionMoveUtil.MoveDirection;
import gov.idaho.isp.saktrack.util.EventUtil;
import java.util.Optional;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestAttribute;
import org.springframework.web.bind.annotation.RequestParam;

@Controller
public class MoveChainOfCustodyController {
  private final SexualAssaultKitRepository sexualAssaultKitRepository;
  private final AuditService auditService;

  public MoveChainOfCustodyController(SexualAssaultKitRepository sexualAssaultKitRepository, AuditService auditService) {
    this.sexualAssaultKitRepository = sexualAssaultKitRepository;
    this.auditService = auditService;
  }

  @PostMapping("/admin/moveEvent")
  public String moveChainOfCustody(@RequestParam Long kitId, @RequestParam Long eventId, @RequestParam MoveDirection direction, @RequestParam String reason, @RequestAttribute User user) {
    SexualAssaultKit kit = sexualAssaultKitRepository.findById(kitId).orElse(null);
    Optional<ChainOfCustodyEvent> event = kit.getChainOfCustody().stream().filter(c -> c.getId().equals(eventId)).findFirst();
    if (event.isPresent()) {
      CollectionMoveUtil.move(kit.getChainOfCustody(), event.get(), direction);
      kit.setQuestionableEvents(EventUtil.hasMissingSendEvents(kit.getChainOfCustody()));
      auditService.auditKit(kit, reason, user);
      sexualAssaultKitRepository.save(kit);
    }
    return "redirect:/admin/manageEvents?kitId=" + kitId;
  }
}
