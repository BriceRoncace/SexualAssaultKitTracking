package gov.idaho.isp.saktrack.controller.organization;

import gov.idaho.isp.saktrack.jurisdiction.JurisdictionRepository;
import gov.idaho.isp.saktrack.organization.OrganizationRepository;
import gov.idaho.isp.saktrack.organization.OrganizationType;
import gov.idaho.isp.saktrack.user.User;
import gov.idaho.isp.saktrack.user.organization.AbstractOrganizationUser;
import gov.idaho.isp.saktrack.user.persistence.OrganizationUserRepository;
import gov.idaho.isp.saktrack.util.RoutingUtil;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestAttribute;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

@Controller
public class LoadOrganizationController {
  private final OrganizationRepository organizationRepository;
  private final OrganizationUserRepository organizationUserRepository;
  private final JurisdictionRepository jurisdictionRepository;

  public LoadOrganizationController(OrganizationRepository organizationRepository, OrganizationUserRepository organizationUserRepository, JurisdictionRepository jurisdictionRepository) {
    this.organizationRepository = organizationRepository;
    this.organizationUserRepository = organizationUserRepository;
    this.jurisdictionRepository = jurisdictionRepository;
  }

  @RequestMapping(value = "/organization/{id}", method = RequestMethod.GET)
  public String loadOrganization(@RequestAttribute User user, @PathVariable Long id, Model model) {
    List<AbstractOrganizationUser> users = organizationUserRepository.findByOrganizationIdOrderByDisplayNameAsc(id);

    model.addAttribute("orgTypes", Arrays.asList(OrganizationType.values()));
    model.addAttribute("jurisdictions", jurisdictionRepository.findAll(new Sort("name")));
    model.addAttribute("organization", organizationRepository.findOne(id));
    model.addAttribute("contactUsers", users.stream().filter(u -> u.isOrganizationContact()).collect(Collectors.toList()));
    model.addAttribute("unverifiedUsers", users.stream().filter(u -> u.getVerifiedDate() == null).collect(Collectors.toList()));
    model.addAttribute("orgUsers", users.stream().filter(u -> u.getVerifiedDate() != null).collect(Collectors.toList()));
    return RoutingUtil.getRoute(user) + "/edit-organization";
  }
}
