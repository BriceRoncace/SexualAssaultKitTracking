package gov.idaho.isp.saktrack.controller.admin.coc;

import gov.idaho.isp.saktrack.SexualAssaultKit;
import gov.idaho.isp.saktrack.controller.BaseController;
import gov.idaho.isp.saktrack.organization.OrganizationRepository;
import gov.idaho.isp.saktrack.persistence.SexualAssaultKitRepository;
import gov.idaho.isp.saktrack.util.EventUtil;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

@Controller
public class ManageChainOfCustodyController extends BaseController {
  private final SexualAssaultKitRepository sexualAssaultKitRepository;
  private final OrganizationRepository organizationRepository;

  public ManageChainOfCustodyController(SexualAssaultKitRepository sexualAssaultKitRepository, OrganizationRepository organizationRepository) {
    this.sexualAssaultKitRepository = sexualAssaultKitRepository;
    this.organizationRepository = organizationRepository;
  }

  @ModelAttribute("kit")
  public SexualAssaultKit loadKit(@RequestParam Long kitId) {
    return sexualAssaultKitRepository.findOne(kitId);
  }

  @RequestMapping(value = "/admin/manageEvents", method = RequestMethod.GET)
  public String manageChainOfCustodyEvents(@ModelAttribute("kit") SexualAssaultKit kit, Model model) {
    model.addAttribute("organizations", organizationRepository.findAssignableOrganizations());
    model.addAttribute("missingSendEvents", EventUtil.getMissingSendEvents(kit.getChainOfCustody()));
    return "/admin/manage-events";
  }
}
