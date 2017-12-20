package gov.idaho.isp.saktrack.controller.admin.coc;

import gov.idaho.isp.saktrack.SexualAssaultKit;
import gov.idaho.isp.saktrack.persistence.SexualAssaultKitRepository;
import gov.idaho.isp.saktrack.service.AuditService;
import gov.idaho.isp.saktrack.user.User;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestAttribute;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

@Controller
public class ClearQuestionableEventsFlagController {
  private final SexualAssaultKitRepository sexualAssaultKitRepository;
  private final AuditService auditService;

  public ClearQuestionableEventsFlagController(SexualAssaultKitRepository sexualAssaultKitRepository, AuditService auditService) {
    this.sexualAssaultKitRepository = sexualAssaultKitRepository;
    this.auditService = auditService;
  }

  @RequestMapping(value = "/admin/clearQuestionable", method = RequestMethod.POST)
  public String clearQuestionableEventsFlag(@RequestParam Long kitId, @RequestParam String reason, @RequestAttribute User user) {
    SexualAssaultKit kit = sexualAssaultKitRepository.findOne(kitId);
    if (kit != null) {
      kit.setQuestionableEvents(false);
    }
    auditService.auditKit(kit, reason, user);
    sexualAssaultKitRepository.save(kit);
    return "redirect:/admin/manageEvents?kitId=" + kitId;
  }
}
