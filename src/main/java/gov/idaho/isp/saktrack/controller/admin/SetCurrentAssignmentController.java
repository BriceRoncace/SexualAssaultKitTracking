package gov.idaho.isp.saktrack.controller.admin;

import gov.idaho.isp.saktrack.SexualAssaultKit;
import gov.idaho.isp.saktrack.controller.BaseController;
import gov.idaho.isp.saktrack.organization.Organization;
import gov.idaho.isp.saktrack.organization.OrganizationRepository;
import gov.idaho.isp.saktrack.persistence.SexualAssaultKitRepository;
import gov.idaho.isp.saktrack.service.AuditService;
import gov.idaho.isp.saktrack.user.User;
import java.util.Optional;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestAttribute;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

@Controller
public class SetCurrentAssignmentController extends BaseController {
  private final SexualAssaultKitRepository sexualAssaultKitRepository;
  private final OrganizationRepository organizationRepository;
  private final AuditService auditService;

  public SetCurrentAssignmentController(SexualAssaultKitRepository sexualAssaultKitRepository, OrganizationRepository organizationRepository, AuditService auditService) {
    this.sexualAssaultKitRepository = sexualAssaultKitRepository;
    this.organizationRepository = organizationRepository;
    this.auditService = auditService;
  }

  @RequestMapping(value = "/admin/setAssignment", method = RequestMethod.POST)
  public String setAssignment(@RequestParam Long id, @RequestParam Optional<Long> currentAssignmentId, @RequestParam String reason, @RequestAttribute User user, Model model, RedirectAttributes ra) {
    SexualAssaultKit kit = sexualAssaultKitRepository.findOne(id);
    kit.setCurrentAssignment(getCurrentAssignmentOrNull(currentAssignmentId));
    auditService.auditKit(kit, reason, user);
    sexualAssaultKitRepository.save(kit);
    ra.addFlashAttribute("messages", getText("kit", kit.getSerialNumber(), " saved"));
    return "redirect:/admin/manageEvents?kitId=" + id;
  }

  private Organization getCurrentAssignmentOrNull(Optional<Long> currentAssignmentId) {
    return currentAssignmentId.isPresent() ? organizationRepository.findOne(currentAssignmentId.get()) : null;
  }
}
