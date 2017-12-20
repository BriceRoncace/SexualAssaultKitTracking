package gov.idaho.isp.saktrack.controller.admin.coc;

import gov.idaho.isp.saktrack.ChainOfCustodyEvent;
import gov.idaho.isp.saktrack.SexualAssaultKit;
import gov.idaho.isp.saktrack.persistence.SexualAssaultKitRepository;
import gov.idaho.isp.saktrack.service.AuditService;
import gov.idaho.isp.saktrack.user.User;
import gov.idaho.isp.saktrack.util.CollectionMoveUtil;
import gov.idaho.isp.saktrack.util.CollectionMoveUtil.MoveDirection;
import gov.idaho.isp.saktrack.util.EventUtil;
import java.util.Optional;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestAttribute;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

@Controller
public class MoveChainOfCustodyController {
  private final SexualAssaultKitRepository sexualAssaultKitRepository;
  private final AuditService auditService;

  public MoveChainOfCustodyController(SexualAssaultKitRepository sexualAssaultKitRepository, AuditService auditService) {
    this.sexualAssaultKitRepository = sexualAssaultKitRepository;
    this.auditService = auditService;
  }

  @RequestMapping(value = "/admin/moveEvent", method = RequestMethod.POST)
  public String moveChainOfCustody(@RequestParam Long kitId, @RequestParam Long eventId, @RequestParam MoveDirection direction, @RequestParam String reason, @RequestAttribute User user) {
    SexualAssaultKit kit = sexualAssaultKitRepository.findOne(kitId);
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
