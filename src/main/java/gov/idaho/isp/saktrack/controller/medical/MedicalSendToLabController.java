package gov.idaho.isp.saktrack.controller.medical;

import gov.idaho.isp.saktrack.controller.BaseController;
import gov.idaho.isp.saktrack.controller.advice.LoadEventDetailsAdvice;
import gov.idaho.isp.saktrack.domain.dto.EventDetails;
import gov.idaho.isp.saktrack.domain.user.User;
import gov.idaho.isp.saktrack.domain.user.organization.MedicalUser;
import gov.idaho.isp.saktrack.exception.SexualAssaultKitTrackingException;
import gov.idaho.isp.saktrack.util.UserUtils;
import gov.idaho.isp.saktrack.validation.group.Single;
import org.springframework.stereotype.Controller;
import org.springframework.validation.BindingResult;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestAttribute;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

@Controller
@LoadEventDetailsAdvice.Apply
public class MedicalSendToLabController extends BaseController {

  @ModelAttribute(binding = false)
  public MedicalUser prepareLoggedInUser(@RequestAttribute User user) {
    UserUtils.verifyUserTypeOrThrowException(user, User.Type.MEDICAL);
    return (MedicalUser) user;
  }

  @PostMapping("/medical/sendToLab")
  public String send(MedicalUser medicalUser, @Validated(Single.class) EventDetails details, BindingResult br, RedirectAttributes ra) {
    if (br.hasErrors()) {
      ra.addFlashAttribute("errors", getErrors(br));
      return "redirect:/medical/dashboard";
    }

    try {
      medicalUser.sendToLab(details);
      ra.addFlashAttribute("messages", getText("kits.transaction", details.getSerialNumbersForMessage(), "sent"));
    }
    catch (SexualAssaultKitTrackingException ex) {
      ra.addFlashAttribute("errors", ex.getErrors());
    }

    return "redirect:/medical/dashboard";
  }
}
