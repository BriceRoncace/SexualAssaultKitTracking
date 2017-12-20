package gov.idaho.isp.saktrack.controller.user;

import gov.idaho.isp.saktrack.organization.OrganizationRepository;
import gov.idaho.isp.saktrack.user.organization.OrganizationUser;
import gov.idaho.isp.saktrack.user.persistence.OrganizationUserRepository;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

@Controller
public class LoadOrganizationUserController {
  private final OrganizationRepository organizationRepository;
  private final OrganizationUserRepository organizationUserRepository;

  public LoadOrganizationUserController(OrganizationRepository organizationRepository, OrganizationUserRepository organizationUserRepository) {
    this.organizationRepository = organizationRepository;
    this.organizationUserRepository = organizationUserRepository;
  }

  @RequestMapping(value = "/organization/{orgId}/user/{userId}", method = RequestMethod.GET)
  public String loadOrganizationUser(@PathVariable Long orgId, @PathVariable Long userId, Model model) {
    OrganizationUser orgUser = organizationUserRepository.findOne(userId);

    if (orgUser != null && orgUser.getOrganization() != null && orgId.equals(orgUser.getOrganization().getId())) {
      model.addAttribute("orgUser", orgUser);
    }
    model.addAttribute("organization", organizationRepository.findOne(orgId));
    return "org-users/save-user";
  }
}
