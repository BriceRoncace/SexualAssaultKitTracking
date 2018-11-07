package gov.idaho.isp.saktrack.controller.organization;

import gov.idaho.isp.saktrack.domain.jurisdiction.JurisdictionRepository;
import gov.idaho.isp.saktrack.domain.organization.OrganizationType;
import gov.idaho.isp.saktrack.domain.user.User;
import gov.idaho.isp.saktrack.util.RoutingUtil;
import java.util.Arrays;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestAttribute;

@Controller
public class NewOrganizationController {
  private final JurisdictionRepository jurisdictionRepository;

  public NewOrganizationController(JurisdictionRepository jurisdictionRepository) {
    this.jurisdictionRepository = jurisdictionRepository;
  }

  @GetMapping("/organization/new")
  public String newOrganization(@RequestAttribute User user, Model model) {
    model.addAttribute("orgTypes", Arrays.asList(OrganizationType.values()));
    model.addAttribute("jurisdictions", jurisdictionRepository.findAll(Sort.by("name")));
    return RoutingUtil.getRoute(user) + "/edit-organization";
  }
}
