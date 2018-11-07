package gov.idaho.isp.saktrack.controller.admin;

import gov.idaho.isp.saktrack.domain.LabDetails;
import gov.idaho.isp.saktrack.domain.MedicalDetails;
import gov.idaho.isp.saktrack.domain.SexualAssaultKit;
import gov.idaho.isp.saktrack.controller.BaseController;
import gov.idaho.isp.saktrack.domain.organization.OrganizationRepository;
import gov.idaho.isp.saktrack.domain.SexualAssaultKitRepository;
import gov.idaho.isp.saktrack.service.AuditService;
import gov.idaho.isp.saktrack.domain.user.User;
import java.util.Optional;
import javax.validation.Valid;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestAttribute;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

@Controller
public class SaveKitController extends BaseController {
  private final SexualAssaultKitRepository sexualAssaultKitRepository;
  private final OrganizationRepository organizationRepository;
  private final AuditService auditService;

  public SaveKitController(SexualAssaultKitRepository sexualAssaultKitRepository, OrganizationRepository organizationRepository, AuditService auditService) {
    this.sexualAssaultKitRepository = sexualAssaultKitRepository;
    this.organizationRepository = organizationRepository;
    this.auditService = auditService;
  }

  @ModelAttribute
  public SexualAssaultKit prepareKit(@RequestParam Long id, @RequestParam Optional<Long> requestingLeAgencyId, @RequestParam Optional<Long> verifiedRequestingLeAgencyId) {
    SexualAssaultKit kit = sexualAssaultKitRepository.findById(id).orElse(null);

    kit.getMedicalDetails().setRequestingLeAgency(null);
    requestingLeAgencyId.ifPresent(agencyId -> {
      if (kit.getMedicalDetails() == null) {
        kit.setMedicalDetails(new MedicalDetails());
      }
      kit.getMedicalDetails().setRequestingLeAgency(organizationRepository.findById(agencyId).orElse(null));
    });

    kit.getLabDetails().setRequestingLeAgency(null);
    verifiedRequestingLeAgencyId.ifPresent(agencyId -> {
      if (kit.getLabDetails() == null) {
        kit.setLabDetails(new LabDetails());
      }
      kit.getLabDetails().setRequestingLeAgency(organizationRepository.findById(agencyId).orElse(null));
    });

    return kit;
  }

  @PostMapping("/admin/saveDetails")
  public String saveDetails(@Valid SexualAssaultKit kit, BindingResult br, @RequestParam String reason, @RequestAttribute User user, Model model, RedirectAttributes ra) {
    if (br.hasErrors()) {
      model.addAttribute("kit", kit);
      model.addAttribute("errors", getErrors(br));
      return "/admin/edit-kit";
    }

    auditService.auditKit(kit, reason, user);
    sexualAssaultKitRepository.save(kit);
    ra.addFlashAttribute("messages", getText("kit", kit.getSerialNumber(), " saved"));
    return "redirect:/admin/edit?id=" + kit.getId();
  }
}