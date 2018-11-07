package gov.idaho.isp.saktrack.controller.admin;

import gov.idaho.isp.saktrack.domain.audit.KitAuditRepository;
import gov.idaho.isp.saktrack.domain.SexualAssaultKitRepository;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

@Controller
public class ViewAuditController {
  private final SexualAssaultKitRepository sexualAssaultKitRepository;
  private final KitAuditRepository kitAuditRepository;

  public ViewAuditController(SexualAssaultKitRepository sexualAssaultKitRepository, KitAuditRepository kitAuditRepository) {
    this.sexualAssaultKitRepository = sexualAssaultKitRepository;
    this.kitAuditRepository = kitAuditRepository;
  }

  @RequestMapping(value = "/admin/viewAudit", method = RequestMethod.GET)
  public String viewChainOfCustody(@RequestParam Long kitId, Model model) {
    model.addAttribute("kit", sexualAssaultKitRepository.findById(kitId).orElse(null));
    model.addAttribute("audits", kitAuditRepository.findByKitIdOrderByModifiedDesc(kitId));
    return "/admin/kit-audits";
  }
}
