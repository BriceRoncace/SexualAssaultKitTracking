package gov.idaho.isp.saktrack.controller.user;

import gov.idaho.isp.saktrack.controller.BaseController;
import gov.idaho.isp.saktrack.domain.user.User;
import gov.idaho.isp.saktrack.domain.user.organization.AbstractOrganizationUser;
import gov.idaho.isp.saktrack.domain.user.organization.OrganizationUserRepository;
import gov.idaho.isp.saktrack.util.UserUtils;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestAttribute;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

@Controller
public class RemoveOrganizationUserController extends BaseController {
  private final OrganizationUserRepository organizationUserRepository;

  public RemoveOrganizationUserController(OrganizationUserRepository organizationUserRepository) {
    this.organizationUserRepository = organizationUserRepository;
  }

  @PostMapping("/organization/{orgId}/user/remove")
  public String removeUser(@RequestParam Long id, RedirectAttributes ra, @RequestAttribute User user) {
    AbstractOrganizationUser orgUser = organizationUserRepository.findById(id).orElse(null);
    organizationUserRepository.delete(orgUser);
    ra.addFlashAttribute("messages", getText("var.remove", orgUser.getDisplayName()));
    return UserUtils.isSameUserEntity(user, orgUser) ? "redirect:/logout" : "redirect:/organization/" + orgUser.getOrganization().getId();
  }
}
