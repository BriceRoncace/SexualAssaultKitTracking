package gov.idaho.isp.saktrack.controller.admin;

import gov.idaho.isp.saktrack.LabDetails;
import gov.idaho.isp.saktrack.MedicalDetails;
import gov.idaho.isp.saktrack.SexualAssaultKit;
import gov.idaho.isp.saktrack.controller.BaseController;
import gov.idaho.isp.saktrack.organization.OrganizationRepository;
import gov.idaho.isp.saktrack.persistence.SexualAssaultKitRepository;
import gov.idaho.isp.saktrack.service.AuditService;
import gov.idaho.isp.saktrack.user.User;
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
    SexualAssaultKit kit = sexualAssaultKitRepository.findOne(id);

    kit.getMedicalDetails().setRequestingLeAgency(null);
    requestingLeAgencyId.ifPresent(agencyId -> {
      if (kit.getMedicalDetails() == null) {
        kit.setMedicalDetails(new MedicalDetails());
      }
      kit.getMedicalDetails().setRequestingLeAgency(organizationRepository.findOne(agencyId));
    });

    kit.getLabDetails().setRequestingLeAgency(null);
    verifiedRequestingLeAgencyId.ifPresent(agencyId -> {
      if (kit.getLabDetails() == null) {
        kit.setLabDetails(new LabDetails());
      }
      kit.getLabDetails().setRequestingLeAgency(organizationRepository.findOne(agencyId));
    });

    return kit;
  }

  @RequestMapping(value = "/admin/saveDetails", method = RequestMethod.POST)
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