package gov.idaho.isp.saktrack.controller.lab;

import gov.idaho.isp.saktrack.controller.BaseController;
import gov.idaho.isp.saktrack.domain.SexualAssaultKit;
import gov.idaho.isp.saktrack.domain.SexualAssaultKitRepository;
import gov.idaho.isp.saktrack.domain.organization.OrganizationRepository;
import java.util.Optional;
import javax.validation.Valid;
import org.springframework.stereotype.Controller;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PostMapping;
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
    SexualAssaultKit kit = sexualAssaultKitRepository.findById(id).orElse(null);
    kit.getLabDetails().setRequestingLeAgency(null);
    if (verifiedRequestingLeAgencyId.isPresent()) {
      kit.getLabDetails().setRequestingLeAgency(organizationRepository.findById(verifiedRequestingLeAgencyId.get()).orElse(null));
    }
    return kit;
  }

  @PostMapping("/lab/saveDetails")
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
