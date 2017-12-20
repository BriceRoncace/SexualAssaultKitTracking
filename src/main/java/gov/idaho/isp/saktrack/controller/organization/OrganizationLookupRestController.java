package gov.idaho.isp.saktrack.controller.organization;

import gov.idaho.isp.saktrack.organization.Organization;
import gov.idaho.isp.saktrack.organization.OrganizationRepository;
import gov.idaho.isp.saktrack.organization.OrganizationType;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
public class OrganizationLookupRestController {
  private final OrganizationRepository organizationRepository;

  public OrganizationLookupRestController(OrganizationRepository organizationRepository) {
    this.organizationRepository = organizationRepository;
  }

  @RequestMapping(value ="/organizations", method = RequestMethod.GET)
  public List<Organization> findOrganizationsByType(@RequestParam(name = "type") OrganizationType... types) {
    return types != null ? findByType(types) : Collections.emptyList();
  }

  private List<Organization> findByType(OrganizationType... types) {
    return Arrays.stream(types)
        .map(organizationRepository::findByTypeOrderByNameAsc)
        .flatMap(l -> l.stream())
        .collect(Collectors.toList());
  }
}