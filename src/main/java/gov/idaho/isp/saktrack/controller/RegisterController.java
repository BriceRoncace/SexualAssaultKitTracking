package gov.idaho.isp.saktrack.controller;

import gov.idaho.isp.saktrack.domain.organization.OrganizationRepository;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

@Controller
public class RegisterController {
  private final OrganizationRepository organizationRepository;

  public RegisterController(OrganizationRepository organizationRepository) {
    this.organizationRepository = organizationRepository;
  }

  @RequestMapping(value = "/register", method = RequestMethod.GET)
  public String loadRegistrationPage(Model model) {
    model.addAttribute("organizations", organizationRepository.findByEnabledOrderByNameAsc(true));
    return "/public/user-registration";
  }
}
