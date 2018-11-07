package gov.idaho.isp.saktrack.controller.user;

import gov.idaho.isp.saktrack.controller.BaseController;
import gov.idaho.isp.saktrack.domain.user.organization.AbstractOrganizationUser;
import gov.idaho.isp.saktrack.domain.user.organization.OrganizationUser;
import gov.idaho.isp.saktrack.domain.user.organization.OrganizationUserRepository;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestAttribute;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

@Controller
public class ChangeOrganizationUserPasskeyController extends BaseController {
  private final OrganizationUserRepository organizationUserRepository;

  public ChangeOrganizationUserPasskeyController(OrganizationUserRepository organizationUserRepository) {
    this.organizationUserRepository = organizationUserRepository;
  }

  @GetMapping("/passkey")
  public String changePasskey(@RequestAttribute OrganizationUser user, Model model) {
    model.addAttribute("organization", user.getOrganization());
    return "public/change-passkey";
  }

  @PostMapping("/passkey")
  public String submitNewPasskey(String passkey, @RequestAttribute AbstractOrganizationUser user, RedirectAttributes ra) {
    if (!user.getOrganization().getPasskey().equals(passkey)) {
      ra.addAttribute("errors", getText("passkey.not.valid"));
      return "redirect:/passkey";
    }

    user.setPasskey(passkey);
    organizationUserRepository.save(user);
    return "redirect:/";
  }
}
