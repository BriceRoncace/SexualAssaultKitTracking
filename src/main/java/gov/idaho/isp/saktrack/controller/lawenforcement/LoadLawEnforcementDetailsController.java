package gov.idaho.isp.saktrack.controller.lawenforcement;

import gov.idaho.isp.saktrack.KitStatus;
import gov.idaho.isp.saktrack.LawEnforcementDetails.NonSubmissionReason;
import gov.idaho.isp.saktrack.SexualAssaultKit;
import gov.idaho.isp.saktrack.organization.OrganizationRepository;
import gov.idaho.isp.saktrack.organization.OrganizationType;
import gov.idaho.isp.saktrack.persistence.SexualAssaultKitRepository;
import gov.idaho.isp.saktrack.user.User;
import gov.idaho.isp.saktrack.user.organization.LawEnforcementUser;
import gov.idaho.isp.saktrack.util.RoutingUtil;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestAttribute;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

@Controller
public class LoadLawEnforcementDetailsController {
  private final SexualAssaultKitRepository sexualAssaultKitRepository;
  private final OrganizationRepository organizationRepository;

  public LoadLawEnforcementDetailsController(SexualAssaultKitRepository sexualAssaultKitRepository, OrganizationRepository organizationRepository) {
    this.sexualAssaultKitRepository = sexualAssaultKitRepository;
    this.organizationRepository = organizationRepository;
  }

  @RequestMapping(value = "/law-enforcement/view", method = RequestMethod.GET)
  public String viewOrEditKit(@RequestParam Long id, Model model, @RequestAttribute User user) {
    SexualAssaultKit kit = sexualAssaultKitRepository.findOne(id);
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
