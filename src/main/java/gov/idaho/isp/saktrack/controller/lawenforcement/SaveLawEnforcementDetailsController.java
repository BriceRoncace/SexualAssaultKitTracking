package gov.idaho.isp.saktrack.controller.lawenforcement;

import gov.idaho.isp.saktrack.LawEnforcementDetails;
import gov.idaho.isp.saktrack.SexualAssaultKit;
import gov.idaho.isp.saktrack.controller.BaseController;
import gov.idaho.isp.saktrack.organization.Organization;
import gov.idaho.isp.saktrack.organization.OrganizationRepository;
import gov.idaho.isp.saktrack.organization.OrganizationType;
import gov.idaho.isp.saktrack.persistence.SexualAssaultKitRepository;
import gov.idaho.isp.saktrack.user.User;
import gov.idaho.isp.saktrack.util.RoutingUtil;
import java.util.Optional;
import javax.validation.Valid;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.RequestAttribute;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

@Controller
public class SaveLawEnforcementDetailsController extends BaseController {
  private final SexualAssaultKitRepository sexualAssaultKitRepository;
  private final OrganizationRepository organizationRepository;

  public SaveLawEnforcementDetailsController(SexualAssaultKitRepository sexualAssaultKitRepository, OrganizationRepository organizationRepository) {
    this.sexualAssaultKitRepository = sexualAssaultKitRepository;
    this.organizationRepository = organizationRepository;
  }

  @ModelAttribute
  public SexualAssaultKit prepareKit(@RequestParam Long id, @RequestParam Optional<Long> reviewingProsecutorOrgId, @RequestAttribute Organization organization) {
    SexualAssaultKit kit = sexualAssaultKitRepository.findOne(id);
    if (organization.isStatewide()) {
      kit.getLegalDetails().setReviewingOrganization(loadOrganizationOrNull(reviewingProsecutorOrgId));
    }
    return kit;
  }

  private Organization loadOrganizationOrNull(Optional<Long> orgId) {
    return orgId.isPresent() ? organizationRepository.findOne(orgId.get()) : null;
  }

  @RequestMapping(value = "/law-enforcement/saveDetails", method = RequestMethod.POST)
  public String saveDetails(@Valid SexualAssaultKit kit, BindingResult br, Model model, RedirectAttributes ra, @RequestAttribute User user) {
    if (br.hasErrors()) {
      model.addAttribute("errors", getErrors(br));
      model.addAttribute("kit", kit);
      model.addAttribute("nonSubmissionReasons", LawEnforcementDetails.NonSubmissionReason.values());
      model.addAttribute("prosecutors", organizationRepository.findByTypeOrderByNameAsc(OrganizationType.LEGAL));
      return RoutingUtil.getLoadKitView(kit, user);
    }

    sexualAssaultKitRepository.save(kit);
    ra.addFlashAttribute("messages", getText("kit", kit.getSerialNumber(), " law enforcement details saved"));
    return "redirect:/law-enforcement/view?id=" + kit.getId();
  }
}