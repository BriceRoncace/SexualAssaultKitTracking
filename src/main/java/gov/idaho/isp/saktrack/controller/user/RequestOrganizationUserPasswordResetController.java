package gov.idaho.isp.saktrack.controller.user;

import gov.idaho.isp.saktrack.controller.BaseController;
import gov.idaho.isp.saktrack.service.PasswordResetService;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

@Controller
public class RequestOrganizationUserPasswordResetController extends BaseController {
  private final PasswordResetService passwordResetService;

  public RequestOrganizationUserPasswordResetController(PasswordResetService passwordResetService) {
    this.passwordResetService = passwordResetService;
  }

  @RequestMapping(value = "/reset/request", method = RequestMethod.POST)
  public String requestPasswordReset(@RequestParam String username, RedirectAttributes redirectAttributes) {
    String error = passwordResetService.requestReset(username);
    if(StringUtils.isNotBlank(error)) {
      redirectAttributes.addFlashAttribute("errors", getText(error));
    }
    else {
      redirectAttributes.addFlashAttribute("messages", getText("password.reset.email.success"));
    }
    return "redirect:/login";
  }
}
