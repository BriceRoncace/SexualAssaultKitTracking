package gov.idaho.isp.saktrack.controller.medical;

import gov.idaho.isp.saktrack.SexualAssaultKit;
import gov.idaho.isp.saktrack.controller.BaseController;
import gov.idaho.isp.saktrack.controller.advice.LoadEventDetailsAdvice.LoadEventDetails;
import gov.idaho.isp.saktrack.dto.EventDetails;
import gov.idaho.isp.saktrack.exception.SexualAssaultKitTrackingException;
import gov.idaho.isp.saktrack.organization.OrganizationRepository;
import gov.idaho.isp.saktrack.persistence.SexualAssaultKitRepository;
import gov.idaho.isp.saktrack.user.User;
import gov.idaho.isp.saktrack.user.UserUtils;
import gov.idaho.isp.saktrack.user.organization.MedicalUser;
import gov.idaho.isp.saktrack.validation.group.Single;
import java.util.Optional;
import javax.validation.Valid;
import org.springframework.stereotype.Controller;
import org.springframework.validation.BindingResult;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.RequestAttribute;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

@Controller
@LoadEventDetails
public class MedicalSendToLawEnforcementController extends BaseController {
  private final SexualAssaultKitRepository sexualAssaultKitRepository;
  private final OrganizationRepository organizationRepository;

  public MedicalSendToLawEnforcementController(SexualAssaultKitRepository sexualAssaultKitRepository, OrganizationRepository organizationRepository) {
    this.sexualAssaultKitRepository = sexualAssaultKitRepository;
    this.organizationRepository = organizationRepository;
  }

  @ModelAttribute(binding = false)
  public MedicalUser prepareLoggedInUser(@RequestAttribute User user) {
    UserUtils.verifyUserTypeOrThrowException(user, User.Type.MEDICAL);
    return (MedicalUser) user;
  }

  @ModelAttribute
  public SexualAssaultKit prepareKit(@RequestParam Long id, @RequestParam Optional<Long> requestingLeAgencyId) {
    SexualAssaultKit kit = sexualAssaultKitRepository.findOne(id);
    if (requestingLeAgencyId.isPresent()) {
      kit.getMedicalDetails().setRequestingLeAgency(organizationRepository.findOne(requestingLeAgencyId.get()));
    }
    else {
      kit.getMedicalDetails().setRequestingLeAgency(null);
    }
    return kit;
  }

  @RequestMapping(value = "/medical/sendToLawEnforcement", method = RequestMethod.POST)
  public String send(@Valid SexualAssaultKit kit, BindingResult br, @Validated(Single.class) EventDetails details, BindingResult br2, MedicalUser medicalUser, RedirectAttributes ra) {
    if (br.hasErrors() || br2.hasErrors()) {
      ra.addFlashAttribute("errors", getErrors(br, br2));
      return "redirect:/medical/dashboard";
    }

    try {
      sexualAssaultKitRepository.save(kit);
      medicalUser.sendToLawEnforcement(details);
      ra.addFlashAttribute("messages", getText("kits.transaction", details.getSerialNumbersForMessage(), "sent"));
    }
    catch (SexualAssaultKitTrackingException ex) {
      ra.addFlashAttribute("errors", ex.getErrors());
    }

    return "redirect:/medical/dashboard";
  }
}