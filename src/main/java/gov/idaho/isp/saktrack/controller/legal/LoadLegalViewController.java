package gov.idaho.isp.saktrack.controller.legal;

import gov.idaho.isp.saktrack.domain.LawEnforcementDetails.NonSubmissionReason;
import gov.idaho.isp.saktrack.domain.SexualAssaultKit;
import gov.idaho.isp.saktrack.domain.SexualAssaultKitRepository;
import gov.idaho.isp.saktrack.domain.user.User;
import gov.idaho.isp.saktrack.util.RoutingUtil;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestAttribute;
import org.springframework.web.bind.annotation.RequestParam;

@Controller
public class LoadLegalViewController {
  private final SexualAssaultKitRepository sexualAssaultKitRepository;

  public LoadLegalViewController(SexualAssaultKitRepository sexualAssaultKitRepository) {
    this.sexualAssaultKitRepository = sexualAssaultKitRepository;
  }

  @PostMapping("/legal/view")
  public String viewOrEditKit(@RequestParam Long id, @RequestAttribute User user, Model model) {
    SexualAssaultKit kit = sexualAssaultKitRepository.findById(id).orElse(null);
    model.addAttribute("kit", kit);
    model.addAttribute("nonSubmissionReasons", NonSubmissionReason.values());
    return RoutingUtil.getLoadKitView(kit, user);
  }
}
