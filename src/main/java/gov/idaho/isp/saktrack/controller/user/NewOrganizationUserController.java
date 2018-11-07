package gov.idaho.isp.saktrack.controller.user;

import gov.idaho.isp.saktrack.domain.organization.OrganizationRepository;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;

@Controller
public class NewOrganizationUserController {
  private final OrganizationRepository organizationRepository;

  public NewOrganizationUserController(OrganizationRepository organizationRepository) {
    this.organizationRepository = organizationRepository;
  }

  @GetMapping("/organization/{orgId}/user/new")
  public String newOrganizationUser(@PathVariable Long orgId, Model model) {
    model.addAttribute("organization", organizationRepository.findById(orgId).orElse(null));
    return "org-users/save-user";
  }
}
