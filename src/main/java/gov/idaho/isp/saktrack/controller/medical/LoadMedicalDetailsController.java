package gov.idaho.isp.saktrack.controller.medical;

import gov.idaho.isp.saktrack.SexualAssaultKit;
import gov.idaho.isp.saktrack.organization.OrganizationRepository;
import gov.idaho.isp.saktrack.organization.OrganizationType;
import gov.idaho.isp.saktrack.persistence.SexualAssaultKitRepository;
import gov.idaho.isp.saktrack.user.User;
import gov.idaho.isp.saktrack.util.RoutingUtil;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestAttribute;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

@Controller
public class LoadMedicalDetailsController {
  private final SexualAssaultKitRepository sexualAssaultKitRepository;
  private final OrganizationRepository organizationRepository;

  public LoadMedicalDetailsController(SexualAssaultKitRepository sexualAssaultKitRepository, OrganizationRepository organizationRepository) {
    this.sexualAssaultKitRepository = sexualAssaultKitRepository;
    this.organizationRepository = organizationRepository;
  }

  @RequestMapping(value = "/medical/view", method = RequestMethod.GET)
  public String viewOrEditKit(@RequestParam Long id, Model model, @RequestAttribute User user) {
    SexualAssaultKit kit = sexualAssaultKitRepository.findOne(id);
    model.addAttribute("kit", kit);
    model.addAttribute("leOrgs", organizationRepository.findByTypeOrderByNameAsc(OrganizationType.LAW_ENFORCEMENT));
    return RoutingUtil.getLoadKitView(kit, user);
  }
}
