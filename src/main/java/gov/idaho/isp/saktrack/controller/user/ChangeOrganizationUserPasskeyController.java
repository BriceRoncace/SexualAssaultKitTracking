package gov.idaho.isp.saktrack.controller.user;

import gov.idaho.isp.saktrack.controller.BaseController;
import gov.idaho.isp.saktrack.user.organization.AbstractOrganizationUser;
import gov.idaho.isp.saktrack.user.organization.OrganizationUser;
import gov.idaho.isp.saktrack.user.persistence.OrganizationUserRepository;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestAttribute;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

@Controller
public class ChangeOrganizationUserPasskeyController extends BaseController {
  private final OrganizationUserRepository organizationUserRepository;

  public ChangeOrganizationUserPasskeyController(OrganizationUserRepository organizationUserRepository) {
    this.organizationUserRepository = organizationUserRepository;
  }

  @RequestMapping(value = "/passkey", method = RequestMethod.GET)
  public String changePasskey(@RequestAttribute OrganizationUser user, Model model) {
    model.addAttribute("organization", user.getOrganization());
    return "public/change-passkey";
  }

  @RequestMapping(value = "/passkey", method = RequestMethod.POST)
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
