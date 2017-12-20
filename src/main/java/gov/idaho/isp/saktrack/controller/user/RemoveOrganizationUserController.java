package gov.idaho.isp.saktrack.controller.user;

import gov.idaho.isp.saktrack.controller.BaseController;
import gov.idaho.isp.saktrack.user.User;
import gov.idaho.isp.saktrack.user.UserUtils;
import gov.idaho.isp.saktrack.user.organization.AbstractOrganizationUser;
import gov.idaho.isp.saktrack.user.persistence.OrganizationUserRepository;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestAttribute;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

@Controller
public class RemoveOrganizationUserController extends BaseController {
  private final OrganizationUserRepository organizationUserRepository;

  public RemoveOrganizationUserController(OrganizationUserRepository organizationUserRepository) {
    this.organizationUserRepository = organizationUserRepository;
  }

  @RequestMapping(value = "/organization/{orgId}/user/remove", method = RequestMethod.POST)
  public String removeUser(@RequestParam Long id, RedirectAttributes ra, @RequestAttribute User user) {
    AbstractOrganizationUser orgUser = organizationUserRepository.findOne(id);
    organizationUserRepository.delete(orgUser);
    ra.addFlashAttribute("messages", getText("var.remove", user.getDisplayName()));
    return UserUtils.isSameUserEntity(user, orgUser) ? "redirect:/logout" : "redirect:/organization/" + orgUser.getOrganization().getId();
  }
}
