package gov.idaho.isp.saktrack.controller.legal;

import gov.idaho.isp.saktrack.LawEnforcementDetails.NonSubmissionReason;
import gov.idaho.isp.saktrack.SexualAssaultKit;
import gov.idaho.isp.saktrack.persistence.SexualAssaultKitRepository;
import gov.idaho.isp.saktrack.user.User;
import gov.idaho.isp.saktrack.util.RoutingUtil;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestAttribute;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

@Controller
public class LoadLegalViewController {
  private final SexualAssaultKitRepository sexualAssaultKitRepository;

  public LoadLegalViewController(SexualAssaultKitRepository sexualAssaultKitRepository) {
    this.sexualAssaultKitRepository = sexualAssaultKitRepository;
  }

  @RequestMapping(value = "/legal/view", method = RequestMethod.GET)
  public String viewOrEditKit(@RequestParam Long id, @RequestAttribute User user, Model model) {
    SexualAssaultKit kit = sexualAssaultKitRepository.findOne(id);
    model.addAttribute("kit", kit);
    model.addAttribute("nonSubmissionReasons", NonSubmissionReason.values());
    return RoutingUtil.getLoadKitView(kit, user);
  }
}
