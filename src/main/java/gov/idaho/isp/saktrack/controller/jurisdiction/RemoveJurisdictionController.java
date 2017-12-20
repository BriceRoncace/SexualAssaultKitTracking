package gov.idaho.isp.saktrack.controller.jurisdiction;

import gov.idaho.isp.saktrack.controller.BaseController;
import gov.idaho.isp.saktrack.jurisdiction.Jurisdiction;
import gov.idaho.isp.saktrack.jurisdiction.JurisdictionRepository;
import gov.idaho.isp.saktrack.organization.Organization;
import gov.idaho.isp.saktrack.organization.OrganizationRepository;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

@Controller
public class RemoveJurisdictionController extends BaseController {
  private final JurisdictionRepository jurisdictionRepository;
  private final OrganizationRepository organizationRepository;

  public RemoveJurisdictionController(JurisdictionRepository jurisdictionRepository, OrganizationRepository organizationRepository) {
    this.jurisdictionRepository = jurisdictionRepository;
    this.organizationRepository = organizationRepository;
  }

  @RequestMapping(value = "/jurisdiction/remove", method = RequestMethod.POST)
  public String listJurisdictions(@RequestParam Optional<Long> jurisdictionId, Model model, RedirectAttributes ra) {
    if (jurisdictionIdIsMissing(jurisdictionId)) {
      return setupModelForFailure(model, "Unable to load jurisdiction.");
    }

    Jurisdiction jurisdiction = jurisdictionRepository.findOne(jurisdictionId.get());

    if (hasOrganizationsWithThisJurisdiction(jurisdiction)) {
      return setupModelForFailure(model, "Unable to remove jurisdiction while in use.");
    }

    String jurisdictionName = jurisdiction.getName();
    jurisdictionRepository.delete(jurisdiction);
    ra.addFlashAttribute("messages", getText("var.remove", jurisdictionName));
    return "redirect:/jurisdictions/list";
  }

  private String setupModelForFailure(Model model, String reasonForFailure) {
    model.addAttribute("jurisdictions", jurisdictionRepository.findAll(new Sort("name")));
    model.addAttribute("jurisdictionTypes", Arrays.asList(Jurisdiction.Type.values()));
    model.addAttribute("errors", reasonForFailure);
    return "admin/jurisdictions";
  }

  private boolean jurisdictionIdIsMissing(Optional<Long> jurisdictionId) {
    return !jurisdictionId.isPresent();
  }

  private boolean hasOrganizationsWithThisJurisdiction(Jurisdiction jurisdiction) {
    List<Organization> orgs = organizationRepository.findByJurisdictionOrderByNameAsc(jurisdiction);
    return !orgs.isEmpty();
  }
}
