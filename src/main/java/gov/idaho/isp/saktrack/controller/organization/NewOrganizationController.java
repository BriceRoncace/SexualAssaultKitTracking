package gov.idaho.isp.saktrack.controller.organization;

import gov.idaho.isp.saktrack.jurisdiction.JurisdictionRepository;
import gov.idaho.isp.saktrack.organization.OrganizationType;
import gov.idaho.isp.saktrack.user.User;
import gov.idaho.isp.saktrack.util.RoutingUtil;
import java.util.Arrays;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestAttribute;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

@Controller
public class NewOrganizationController {
  private final JurisdictionRepository jurisdictionRepository;

  public NewOrganizationController(JurisdictionRepository jurisdictionRepository) {
    this.jurisdictionRepository = jurisdictionRepository;
  }
  
  @RequestMapping(value = "/organization/new", method = RequestMethod.GET)
  public String newOrganization(@RequestAttribute User user, Model model) {
    model.addAttribute("orgTypes", Arrays.asList(OrganizationType.values()));
    model.addAttribute("jurisdictions", jurisdictionRepository.findAll(new Sort("name")));
    return RoutingUtil.getRoute(user) + "/edit-organization";
  }
}
