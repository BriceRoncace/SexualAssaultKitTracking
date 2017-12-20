package gov.idaho.isp.saktrack.controller.legal;

import gov.idaho.isp.saktrack.SexualAssaultKit;
import gov.idaho.isp.saktrack.controller.BaseController;
import gov.idaho.isp.saktrack.exception.SexualAssaultKitTrackingException;
import gov.idaho.isp.saktrack.persistence.SexualAssaultKitRepository;
import gov.idaho.isp.saktrack.service.EmailService;
import gov.idaho.isp.saktrack.user.User;
import gov.idaho.isp.saktrack.user.UserUtils;
import gov.idaho.isp.saktrack.user.organization.LegalUser;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.RequestAttribute;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

@Controller
public class LegalReviewController extends BaseController {
  private final SexualAssaultKitRepository sexualAssaultKitRepository;

  public LegalReviewController(SexualAssaultKitRepository sexualAssaultKitRepository, EmailService emailService) {
    this.sexualAssaultKitRepository = sexualAssaultKitRepository;
  }

  @ModelAttribute
  public SexualAssaultKit prepareKit(@RequestParam(value = "kitId") Long id) {
    return sexualAssaultKitRepository.findOne(id);
  }

  @ModelAttribute
  public LegalUser prepareLoggedInUser(@RequestAttribute User user) {
    UserUtils.verifyUserTypeOrThrowException(user, User.Type.LEGAL);
    return (LegalUser) user;
  }

  @RequestMapping(value = "/legal/agree", method = RequestMethod.POST)
  public String agree(SexualAssaultKit kit, String notes, LegalUser legalUser, RedirectAttributes ra) {
    try {
      legalUser.agreeWithNonSubmission(kit, notes);
      ra.addFlashAttribute("messages", getText("kit", kit.getSerialNumber(), ": Prosectuor agreed with non-submission"));
      return "redirect:/legal/dashboard";
    }
    catch (SexualAssaultKitTrackingException ex) {
      ra.addFlashAttribute("errors", ex.getErrors());
      return "redirect:/legal/view?id=" + kit.getId();
    }
  }

  @RequestMapping(value = "/legal/disagree", method = RequestMethod.POST)
  public String disagree(SexualAssaultKit kit, String notes, LegalUser legalUser, RedirectAttributes ra) {
    try {
      legalUser.disagreeWithNonSubmission(kit, notes);
      ra.addFlashAttribute("messages", getText("kit", kit.getSerialNumber(), ": Prosectuor disagreed with non-submission"));
      return "redirect:/legal/dashboard";
    }
    catch (SexualAssaultKitTrackingException ex) {
      ra.addFlashAttribute("errors", ex.getErrors());
      return "redirect:/legal/view?id=" + kit.getId();
    }
  }
}