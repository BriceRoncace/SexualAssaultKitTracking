package gov.idaho.isp.saktrack.controller.user;

import gov.idaho.isp.saktrack.domain.organization.OrganizationRepository;
import gov.idaho.isp.saktrack.domain.user.organization.OrganizationUser;
import gov.idaho.isp.saktrack.domain.user.organization.OrganizationUserRepository;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;

@Controller
public class LoadOrganizationUserController {
  private final OrganizationRepository organizationRepository;
  private final OrganizationUserRepository organizationUserRepository;

  public LoadOrganizationUserController(OrganizationRepository organizationRepository, OrganizationUserRepository organizationUserRepository) {
    this.organizationRepository = organizationRepository;
    this.organizationUserRepository = organizationUserRepository;
  }

  @GetMapping("/organization/{orgId}/user/{userId}")
  public String loadOrganizationUser(@PathVariable Long orgId, @PathVariable Long userId, Model model) {
    OrganizationUser orgUser = organizationUserRepository.findById(userId).orElse(null);

    if (orgUser != null && orgUser.getOrganization() != null && orgId.equals(orgUser.getOrganization().getId())) {
      model.addAttribute("orgUser", orgUser);
    }
    model.addAttribute("organization", organizationRepository.findById(orgId).orElse(null));
    return "org-users/save-user";
  }
}
