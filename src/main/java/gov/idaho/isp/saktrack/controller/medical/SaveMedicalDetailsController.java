package gov.idaho.isp.saktrack.controller.medical;

import gov.idaho.isp.saktrack.SexualAssaultKit;
import gov.idaho.isp.saktrack.controller.BaseController;
import gov.idaho.isp.saktrack.organization.OrganizationRepository;
import gov.idaho.isp.saktrack.organization.OrganizationType;
import gov.idaho.isp.saktrack.persistence.SexualAssaultKitRepository;
import gov.idaho.isp.saktrack.user.User;
import gov.idaho.isp.saktrack.util.RoutingUtil;
import java.util.Optional;
import javax.validation.Valid;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.RequestAttribute;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

@Controller
public class SaveMedicalDetailsController extends BaseController {
  private final SexualAssaultKitRepository sexualAssaultKitRepository;
  private final OrganizationRepository organizationRepository;

  public SaveMedicalDetailsController(SexualAssaultKitRepository sexualAssaultKitRepository, OrganizationRepository organizationRepository) {
    this.sexualAssaultKitRepository = sexualAssaultKitRepository;
    this.organizationRepository = organizationRepository;
  }

  @ModelAttribute
  public SexualAssaultKit prepareKit(@RequestParam Long id, @RequestParam Optional<Long> requestingLeAgencyId) {
    SexualAssaultKit kit = sexualAssaultKitRepository.findOne(id);
    kit.getMedicalDetails().setRequestingLeAgency(null);
    if (requestingLeAgencyId.isPresent()) {
      kit.getMedicalDetails().setRequestingLeAgency(organizationRepository.findOne(requestingLeAgencyId.get()));
    }
    return kit;
  }

  @RequestMapping(value = "/medical/saveDetails", method = RequestMethod.POST)
  public String saveDetails(@Valid SexualAssaultKit kit, BindingResult br, Model model, RedirectAttributes ra, @RequestAttribute User user) {
    if (br.hasErrors()) {
      model.addAttribute("errors", getErrors(br));
      model.addAttribute("kit", kit);
      model.addAttribute("leOrgs", organizationRepository.findByTypeOrderByNameAsc(OrganizationType.LAW_ENFORCEMENT));
      return RoutingUtil.getLoadKitView(kit, user);
    }
    sexualAssaultKitRepository.save(kit);
    ra.addFlashAttribute("messages", getText("kit", kit.getSerialNumber(), " medical details saved"));
    return "redirect:/medical/view?id=" + kit.getId();
  }
}