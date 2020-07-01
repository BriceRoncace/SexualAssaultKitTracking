package gov.idaho.isp.saktrack.controller.lawenforcement;

import gov.idaho.isp.saktrack.controller.BaseController;
import gov.idaho.isp.saktrack.controller.advice.LoadEventDetailsAdvice;
import gov.idaho.isp.saktrack.domain.dto.EventDetails;
import gov.idaho.isp.saktrack.domain.organization.Organization;
import gov.idaho.isp.saktrack.domain.organization.OrganizationRepository;
import gov.idaho.isp.saktrack.domain.organization.OrganizationType;
import gov.idaho.isp.saktrack.domain.user.User;
import gov.idaho.isp.saktrack.domain.user.organization.LawEnforcementUser;
import gov.idaho.isp.saktrack.exception.SexualAssaultKitTrackingException;
import gov.idaho.isp.saktrack.util.UserUtils;
import gov.idaho.isp.saktrack.validation.group.Batch;
import org.springframework.stereotype.Controller;
import org.springframework.validation.BindingResult;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestAttribute;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

@Controller
@LoadEventDetailsAdvice.Apply
public class LawEnforcementBatchSendController extends BaseController {
  private final OrganizationRepository organizationRepository;

  public LawEnforcementBatchSendController(OrganizationRepository organizationRepository) {
    this.organizationRepository = organizationRepository;
  }

  @ModelAttribute(binding = false)
  public LawEnforcementUser prepareLoggedInUser(@RequestAttribute User user) {
    UserUtils.verifyUserTypeOrThrowException(user, User.Type.LAW_ENFORCEMENT);
    return (LawEnforcementUser) user;
  }

  @ModelAttribute(binding = false)
  public Organization prepareDestinationOrganization(@RequestParam Long orgId) {
    return organizationRepository.findById(orgId).orElse(null);
  }

  @PostMapping("/law-enforcement/batchSend")
  public String sendKits(LawEnforcementUser leUser, Organization destinationOrganization, @Validated(Batch.class) EventDetails details, BindingResult br, RedirectAttributes ra) {
    if (br.hasErrors()) {
      ra.addFlashAttribute("errors", getErrors(br));
      return "redirect:/law-enforcement/dashboard";
    }

    try {
      if (destinationOrganization.getType() == OrganizationType.LAB) {
        leUser.batchSendToLab(details);
      }
      if (destinationOrganization.getType() == OrganizationType.MEDICAL) {
        leUser.batchSendToMedical(details);
      }
      if (destinationOrganization.getType() == OrganizationType.LAW_ENFORCEMENT) {
        leUser.batchSendToLawEnforcement(details);
      }

      ra.addFlashAttribute("messages", getText("kits.transaction", details.getSerialNumbersForMessage(), "sent"));
    }
    catch (SexualAssaultKitTrackingException ex) {
      ra.addFlashAttribute("errors", ex.getErrors());
    }

    return "redirect:/law-enforcement/dashboard";
  }
}
