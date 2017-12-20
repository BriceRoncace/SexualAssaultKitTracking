package gov.idaho.isp.saktrack.controller.user;

import gov.idaho.isp.saktrack.organization.OrganizationRepository;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

@Controller
public class NewOrganizationUserController {
  private final OrganizationRepository organizationRepository;

  public NewOrganizationUserController(OrganizationRepository organizationRepository) {
    this.organizationRepository = organizationRepository;
  }

  @RequestMapping(value = "/organization/{orgId}/user/new", method = RequestMethod.GET)
  public String newOrganizationUser(@PathVariable Long orgId, Model model) {
    model.addAttribute("organization", organizationRepository.findOne(orgId));
    return "org-users/save-user";
  }
}
