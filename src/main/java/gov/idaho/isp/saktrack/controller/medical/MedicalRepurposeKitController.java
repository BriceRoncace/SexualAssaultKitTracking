package gov.idaho.isp.saktrack.controller.medical;

import gov.idaho.isp.saktrack.controller.BaseController;
import gov.idaho.isp.saktrack.controller.advice.LoadEventDetailsAdvice.LoadEventDetails;
import gov.idaho.isp.saktrack.dto.EventDetails;
import gov.idaho.isp.saktrack.exception.SexualAssaultKitTrackingException;
import gov.idaho.isp.saktrack.user.User;
import gov.idaho.isp.saktrack.user.UserUtils;
import gov.idaho.isp.saktrack.user.organization.MedicalUser;
import gov.idaho.isp.saktrack.validation.group.Single;
import org.springframework.stereotype.Controller;
import org.springframework.validation.BindingResult;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.RequestAttribute;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

@Controller
@LoadEventDetails
public class MedicalRepurposeKitController extends BaseController {

  @ModelAttribute
  public MedicalUser prepareLoggedInUser(@RequestAttribute User user) {
    UserUtils.verifyUserTypeOrThrowException(user, User.Type.MEDICAL);
    return (MedicalUser) user;
  }

  @RequestMapping(value = "/medical/repurpose", method = RequestMethod.POST)
  public String repurpose(MedicalUser medicalUser, @Validated(Single.class) EventDetails details, BindingResult br, RedirectAttributes ra) {
    if (br.hasErrors()) {
      ra.addFlashAttribute("errors", getErrors(br));
      return "redirect:/medical/dashboard";
    }

    try {
      medicalUser.repurpose(details);
      ra.addFlashAttribute("messages", getText("kits.transaction", details.getSerialNumbersForMessage(), "repurposed"));
    }
    catch (SexualAssaultKitTrackingException ex) {
      ra.addFlashAttribute("errors", ex.getErrors());
    }

    return "redirect:/";
  }
}
