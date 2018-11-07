package gov.idaho.isp.saktrack.controller.lawenforcement;

import gov.idaho.isp.saktrack.domain.LawEnforcementDetails;
import gov.idaho.isp.saktrack.domain.SexualAssaultKit;
import gov.idaho.isp.saktrack.controller.BaseController;
import gov.idaho.isp.saktrack.domain.organization.Organization;
import gov.idaho.isp.saktrack.domain.organization.OrganizationRepository;
import gov.idaho.isp.saktrack.domain.organization.OrganizationType;
import gov.idaho.isp.saktrack.domain.SexualAssaultKitRepository;
import gov.idaho.isp.saktrack.domain.user.organization.LawEnforcementUser;
import gov.idaho.isp.saktrack.util.RoutingUtil;
import java.util.Optional;
import javax.validation.Valid;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestAttribute;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

@Controller
public class ReleaseKitForReviewController extends BaseController {
  private final SexualAssaultKitRepository sexualAssaultKitRepository;
  private final OrganizationRepository organizationRepository;

  public ReleaseKitForReviewController(SexualAssaultKitRepository sexualAssaultKitRepository, OrganizationRepository organizationRepository) {
    this.sexualAssaultKitRepository = sexualAssaultKitRepository;
    this.organizationRepository = organizationRepository;
  }

  @ModelAttribute
  public SexualAssaultKit prepareKit(@RequestParam Long id, @RequestParam Optional<Long> reviewingProsecutorOrgId, @RequestAttribute Organization organization) {
    SexualAssaultKit kit = sexualAssaultKitRepository.findById(id).orElse(null);
    if (organization.isStatewide()) {
      kit.getLegalDetails().setReviewingOrganization(loadOrganizationOrNull(reviewingProsecutorOrgId));
    }
    return kit;
  }

  private Organization loadOrganizationOrNull(Optional<Long> orgId) {
    return orgId.isPresent() ? organizationRepository.findById(orgId.get()).orElse(null) : null;
  }

  @PostMapping("/law-enforcement/releaseKitForReview")
  public String releaseKitForReview(@Valid SexualAssaultKit kit, BindingResult br, Model model, RedirectAttributes ra, @RequestAttribute LawEnforcementUser user) {
    if (br.hasErrors()) {
      model.addAttribute("errors", getErrors(br));
      model.addAttribute("kit", kit);
      model.addAttribute("nonSubmissionReasons", LawEnforcementDetails.NonSubmissionReason.values());
      model.addAttribute("prosecutors", organizationRepository.findByTypeOrderByNameAsc(OrganizationType.LEGAL));
      return RoutingUtil.getLoadKitView(kit, user);
    }

    user.releaseForProsecutorReview(kit);
    ra.addFlashAttribute("messages", getText("kit", kit.getSerialNumber(), " details saved and is now reviewable by prosecutor"));
    return "redirect:/law-enforcement/view?id=" + kit.getId();
  }
}
