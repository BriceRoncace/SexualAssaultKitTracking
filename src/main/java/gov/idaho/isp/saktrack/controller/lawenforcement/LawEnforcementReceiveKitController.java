package gov.idaho.isp.saktrack.controller.lawenforcement;

import gov.idaho.isp.saktrack.controller.BaseController;
import gov.idaho.isp.saktrack.controller.advice.LoadEventDetailsAdvice.LoadEventDetails;
import gov.idaho.isp.saktrack.domain.dto.EventDetails;
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
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

@Controller
@LoadEventDetails
public class LawEnforcementReceiveKitController extends BaseController {

  @ModelAttribute
  public LawEnforcementUser prepareLoggedInUser(@RequestAttribute User user) {
    UserUtils.verifyUserTypeOrThrowException(user, User.Type.LAW_ENFORCEMENT);
    return (LawEnforcementUser) user;
  }

  @PostMapping("/law-enforcement/receive")
  public String batchReceive(LawEnforcementUser lawUser, @Validated(Batch.class) EventDetails details, BindingResult bindingResult, RedirectAttributes ra) {
    if (bindingResult.hasErrors()) {
      ra.addFlashAttribute("errors", getErrors(bindingResult));
      return "redirect:/law-enforcement/dashboard";
    }

    try {
      lawUser.batchReceive(details);
      ra.addFlashAttribute("messages", getText("kits.transaction", details.getSerialNumbersForMessage(), "received"));
    }
    catch (SexualAssaultKitTrackingException ex) {
      ra.addFlashAttribute("errors", ex.getErrors());
    }
    return "redirect:/law-enforcement/dashboard";
  }
}
