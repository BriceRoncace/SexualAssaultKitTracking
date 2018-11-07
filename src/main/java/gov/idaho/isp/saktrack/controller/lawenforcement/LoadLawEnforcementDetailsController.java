package gov.idaho.isp.saktrack.controller.lawenforcement;

import gov.idaho.isp.saktrack.domain.KitStatus;
import gov.idaho.isp.saktrack.domain.LawEnforcementDetails.NonSubmissionReason;
import gov.idaho.isp.saktrack.domain.SexualAssaultKit;
import gov.idaho.isp.saktrack.domain.SexualAssaultKitRepository;
import gov.idaho.isp.saktrack.domain.organization.OrganizationRepository;
import gov.idaho.isp.saktrack.domain.organization.OrganizationType;
import gov.idaho.isp.saktrack.domain.user.User;
import gov.idaho.isp.saktrack.domain.user.organization.LawEnforcementUser;
import gov.idaho.isp.saktrack.util.RoutingUtil;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestAttribute;
import org.springframework.web.bind.annotation.RequestParam;

@Controller
public class LoadLawEnforcementDetailsController {
  private final SexualAssaultKitRepository sexualAssaultKitRepository;
  private final OrganizationRepository organizationRepository;

  public LoadLawEnforcementDetailsController(SexualAssaultKitRepository sexualAssaultKitRepository, OrganizationRepository organizationRepository) {
    this.sexualAssaultKitRepository = sexualAssaultKitRepository;
    this.organizationRepository = organizationRepository;
  }

  @GetMapping("/law-enforcement/view")
  public String viewOrEditKit(@RequestParam Long id, Model model, @RequestAttribute User user) {
    SexualAssaultKit kit = sexualAssaultKitRepository.findById(id).orElse(null);
    model.addAttribute("kit", kit);
    model.addAttribute("nonSubmissionReasons", NonSubmissionReason.values());
    model.addAttribute("prosecutors", organizationRepository.findByTypeOrderByNameAsc(OrganizationType.LEGAL));
    model.addAttribute("disableSendKitButton", shouldDisableSendButton(kit, user));
    return RoutingUtil.getLoadKitView(kit, user);
  }

  private boolean shouldDisableSendButton(SexualAssaultKit kit, User user) {
    if (user.getType().equals(User.Type.LAW_ENFORCEMENT)) {
      LawEnforcementUser leUser = (LawEnforcementUser) user;
      return leUser.isRequestingAgencyMyAgency(kit) && !KitStatus.READY_TO_SEND_FOR_ANALYSIS.equals(kit.getStatus());
    }
    else {
      return user.isAdmin();
    }
  }
}
