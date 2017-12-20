package gov.idaho.isp.saktrack.controller.lab;

import gov.idaho.isp.saktrack.SexualAssaultKit;
import gov.idaho.isp.saktrack.controller.BaseController;
import gov.idaho.isp.saktrack.organization.OrganizationRepository;
import gov.idaho.isp.saktrack.persistence.SexualAssaultKitRepository;
import java.util.Optional;
import javax.validation.Valid;
import org.springframework.stereotype.Controller;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

@Controller
public class SaveLabDetailsController extends BaseController {
  private final SexualAssaultKitRepository sexualAssaultKitRepository;
  private final OrganizationRepository organizationRepository;

  public SaveLabDetailsController(SexualAssaultKitRepository sexualAssaultKitRepository, OrganizationRepository organizationRepository) {
    this.sexualAssaultKitRepository = sexualAssaultKitRepository;
    this.organizationRepository = organizationRepository;
  }

  @ModelAttribute
  public SexualAssaultKit prepareKit(@RequestParam Long id, @RequestParam Optional<Long> verifiedRequestingLeAgencyId) {
    SexualAssaultKit kit = sexualAssaultKitRepository.findOne(id);
    kit.getLabDetails().setRequestingLeAgency(null);
    if (verifiedRequestingLeAgencyId.isPresent()) {
      kit.getLabDetails().setRequestingLeAgency(organizationRepository.findOne(verifiedRequestingLeAgencyId.get()));
    }
    return kit;
  }

  @RequestMapping(value = "/lab/saveDetails", method = RequestMethod.POST)
  public String saveDetails(@Valid SexualAssaultKit kit, BindingResult bindingResult, RedirectAttributes ra) {
    if (bindingResult.hasErrors()) {
      ra.addFlashAttribute("kit", kit);
      ra.addFlashAttribute("errors", getErrors(bindingResult));
      return "redirect:/lab/view?id=" + kit.getId();
    }

    sexualAssaultKitRepository.save(kit);
    ra.addFlashAttribute("messages", getText("kit", kit.getSerialNumber(), " lab details saved"));
    return "redirect:/lab/view?id=" + kit.getId();
  }
}
