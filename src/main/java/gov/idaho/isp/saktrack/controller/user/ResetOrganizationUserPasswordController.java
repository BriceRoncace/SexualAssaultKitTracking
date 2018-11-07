package gov.idaho.isp.saktrack.controller.user;

import gov.idaho.isp.saktrack.controller.BaseController;
import gov.idaho.isp.saktrack.domain.user.password.dto.ResetPasswordPair;
import gov.idaho.isp.saktrack.service.PasswordResetService;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

@Controller
public class ResetOrganizationUserPasswordController extends BaseController {
  private final PasswordResetService passwordResetService;

  public ResetOrganizationUserPasswordController(PasswordResetService passwordResetService) {
    this.passwordResetService = passwordResetService;
  }

  @GetMapping("/reset/password")
  public String loadPasswordResetRequest(@RequestParam String request, Model model) {
    if (!passwordResetService.isRequestValid(request)) {
      model.addAttribute("errors", getText("password.reset.invalid"));
      return "public/login";
    }

    model.addAttribute("request", request);
    return "/public/password-reset";
  }

  @PostMapping("/reset/password")
  public String savePasswordResetRequest(@Validated ResetPasswordPair passwordPair, BindingResult br, Model model, RedirectAttributes ra) {
    if (br.hasErrors()) {
      model.addAttribute("errors", getErrors(br));
      model.addAttribute("request", passwordPair.getRequest());
      return "/public/password-reset";
    }

    if (passwordResetService.resetPassword(passwordPair)) {
      ra.addFlashAttribute("messages", getText("password.reset.success"));
    }
    else {
      ra.addFlashAttribute("errors", getText("password.reset.invalid"));
    }
    return "redirect:/login";
  }
}
