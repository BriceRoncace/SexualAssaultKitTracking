package gov.idaho.isp.saktrack.controller.lab;

import gov.idaho.isp.saktrack.controller.BaseController;
import gov.idaho.isp.saktrack.domain.SexualAssaultKit;
import gov.idaho.isp.saktrack.domain.SexualAssaultKitRepository;
import gov.idaho.isp.saktrack.domain.user.User;
import gov.idaho.isp.saktrack.domain.user.organization.LabUser;
import gov.idaho.isp.saktrack.service.SerialNumberFormatter;
import gov.idaho.isp.saktrack.util.UserUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.web.PageableDefault;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.RequestAttribute;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

@Controller
public class LabQuickSearchController extends BaseController {
  private final SexualAssaultKitRepository sexualAssaultKitRepository;
  private final SerialNumberFormatter serialNumberFormatter;

  public LabQuickSearchController(SexualAssaultKitRepository sexualAssaultKitRepository, SerialNumberFormatter serialNumberFormatter) {
    this.sexualAssaultKitRepository = sexualAssaultKitRepository;
    this.serialNumberFormatter = serialNumberFormatter;
  }

  @ModelAttribute
  public LabUser prepareLoggedInUser(@RequestAttribute User user) {
    UserUtils.verifyUserTypeOrThrowException(user, User.Type.LAB);
    return (LabUser) user;
  }

  @GetMapping("/lab/quickSearch")
  public String quickSearch(LabUser user, String searchText, @PageableDefault(size=50, sort = "lastModified", direction = Sort.Direction.DESC) Pageable pageable, Model model, RedirectAttributes ra) {
    Page<SexualAssaultKit> kits = sexualAssaultKitRepository.findBySerialNumberOrLabDetailsCaseNumber(formatAsSerialNumber(searchText), searchText, pageable);
    if (kits != null && kits.hasContent()) {
      if (kits.getTotalElements() == 1L) {
        return "redirect:/timeline?serialNumber=" + kits.getContent().get(0).getSerialNumber();
      }
      model.addAttribute("page", kits);
      return "lab/kit-quick-search";
    }
    ra.addFlashAttribute("errors", getText("kits.not.found", searchText));
    return "redirect:/lab/dashboard";
  }

  private String formatAsSerialNumber(String string) {
    if (serialNumberFormatter.isValid(string)) {
      return serialNumberFormatter.format(string);
    }
    return null;
  }
}
