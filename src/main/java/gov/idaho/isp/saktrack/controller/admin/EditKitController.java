package gov.idaho.isp.saktrack.controller.admin;

import gov.idaho.isp.saktrack.LawEnforcementDetails;
import gov.idaho.isp.saktrack.SexualAssaultKit;
import gov.idaho.isp.saktrack.organization.OrganizationRepository;
import gov.idaho.isp.saktrack.organization.OrganizationType;
import gov.idaho.isp.saktrack.persistence.SexualAssaultKitRepository;
import java.util.Optional;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

@Controller
public class EditKitController {
  private final SexualAssaultKitRepository sexualAssaultKitRepository;
  private final OrganizationRepository organizationRepository;

  public EditKitController(SexualAssaultKitRepository sexualAssaultKitRepository, OrganizationRepository organizationRepository) {
    this.sexualAssaultKitRepository = sexualAssaultKitRepository;
    this.organizationRepository = organizationRepository;
  }

  @ModelAttribute
  public SexualAssaultKit prepareKit(@RequestParam Long id, @RequestParam Optional<Long> requestingLeAgencyId, @RequestParam Optional<Long> verifiedRequestingLeAgencyId) {
    return sexualAssaultKitRepository.findOne(id);
  }

  @RequestMapping(value = {"/admin/view", "/admin/edit"}, method = RequestMethod.GET)
  public String editDetails(SexualAssaultKit kit, Model model) {
    model.addAttribute("kit", kit);
    model.addAttribute("leOrgs", organizationRepository.findByTypeOrderByNameAsc(OrganizationType.LAW_ENFORCEMENT));
    model.addAttribute("medOrgs", organizationRepository.findByTypeOrderByNameAsc(OrganizationType.MEDICAL));
    model.addAttribute("nonSubmissionReasons", LawEnforcementDetails.NonSubmissionReason.values());
    model.addAttribute("prosecutors", organizationRepository.findByTypeOrderByNameAsc(OrganizationType.LEGAL));
    return "/admin/edit-kit";
  }
}
