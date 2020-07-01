package gov.idaho.isp.saktrack.controller.medical;

import gov.idaho.isp.saktrack.controller.BaseController;
import gov.idaho.isp.saktrack.controller.advice.LoadEventDetailsAdvice;
import gov.idaho.isp.saktrack.domain.dto.EventDetails;
import gov.idaho.isp.saktrack.domain.organization.Organization;
import gov.idaho.isp.saktrack.domain.organization.OrganizationRepository;
import gov.idaho.isp.saktrack.domain.organization.OrganizationType;
import gov.idaho.isp.saktrack.domain.user.User;
import gov.idaho.isp.saktrack.domain.user.organization.MedicalUser;
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
public class MedicalBatchSendController extends BaseController {
  private final OrganizationRepository organizationRepository;

  public MedicalBatchSendController(OrganizationRepository organizationRepository) {
    this.organizationRepository = organizationRepository;
  }

  @ModelAttribute(binding = false)
  public MedicalUser prepareLoggedInUser(@RequestAttribute User user) {
    UserUtils.verifyUserTypeOrThrowException(user, User.Type.MEDICAL);
    return (MedicalUser) user;
  }

  @ModelAttribute(binding = false)
  public Organization prepareDestinationOrganization(@RequestParam Long orgId) {
    return organizationRepository.findById(orgId).orElse(null);
  }

  @PostMapping("/medical/batchSend")
  public String sendKits(MedicalUser medicalUser, Organization destinationOrganization, @Validated(Batch.class) EventDetails details, BindingResult br, RedirectAttributes ra) {
    if (br.hasErrors()) {
      ra.addFlashAttribute("errors", getErrors(br));
      return "redirect:/lab/dashboard";
    }

    try {
      if (destinationOrganization.getType() == OrganizationType.LAB) {
        medicalUser.batchSendToLab(details);
      }
      if (destinationOrganization.getType() == OrganizationType.MEDICAL) {
        medicalUser.batchSendToMedical(details);
      }
      if (destinationOrganization.getType() == OrganizationType.LAW_ENFORCEMENT) {
        medicalUser.batchSendToLawEnforcement(details);
      }

      ra.addFlashAttribute("messages", getText("kits.transaction", details.getSerialNumbersForMessage(), "sent"));
    }
    catch (SexualAssaultKitTrackingException ex) {
      ra.addFlashAttribute("errors", ex.getErrors());
    }

    return "redirect:/medical/dashboard";
  }
}
