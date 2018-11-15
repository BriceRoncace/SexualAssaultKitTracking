package gov.idaho.isp.saktrack.controller.lab;

import gov.idaho.isp.saktrack.controller.BaseController;
import gov.idaho.isp.saktrack.controller.advice.LoadEventDetailsAdvice;
import gov.idaho.isp.saktrack.domain.dto.EventDetails;
import gov.idaho.isp.saktrack.domain.user.User;
import gov.idaho.isp.saktrack.domain.user.organization.LabUser;
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
public class LabReturnToLawEnforcementController extends BaseController {

  @ModelAttribute
  public LabUser prepareLoggedInUser(@RequestAttribute User user) {
    UserUtils.verifyUserTypeOrThrowException(user, User.Type.LAB);
    return (LabUser) user;
  }

  @PostMapping("/lab/returnToLawEnforcement")
  public String returnToLawEnforcement(LabUser labUser, @Validated(Single.class) EventDetails details, BindingResult bindingResult, RedirectAttributes ra) {
    if (bindingResult.hasErrors()) {
      ra.addFlashAttribute("errors", getErrors(bindingResult));
      return "redirect:/lab/dashboard";
    }

    try {
      labUser.returnToLawEnforcement(details);
      ra.addFlashAttribute("messages", getText("kits.transaction", details.getSerialNumbersForMessage(), "returned"));
    }
    catch (SexualAssaultKitTrackingException ex) {
      ra.addFlashAttribute("errors", ex.getErrors());
    }
    return "redirect:/lab/dashboard";
  }
}
