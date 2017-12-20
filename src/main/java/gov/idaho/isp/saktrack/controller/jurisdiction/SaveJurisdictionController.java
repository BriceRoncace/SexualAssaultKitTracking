package gov.idaho.isp.saktrack.controller.jurisdiction;

import gov.idaho.isp.saktrack.controller.BaseController;
import gov.idaho.isp.saktrack.jurisdiction.Jurisdiction;
import gov.idaho.isp.saktrack.jurisdiction.JurisdictionRepository;
import gov.idaho.isp.saktrack.user.User;
import java.util.Arrays;
import java.util.Optional;
import javax.validation.Valid;
import org.springframework.data.domain.Sort;
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
public class SaveJurisdictionController extends BaseController {
  private final JurisdictionRepository jurisdictionRepository;

  public SaveJurisdictionController(JurisdictionRepository jurisdictionRepository) {
    this.jurisdictionRepository = jurisdictionRepository;
  }

  @ModelAttribute
  public Jurisdiction loadJurisdiction(@RequestParam Optional<Long> jurisdictionId) {
    return jurisdictionId.isPresent() ? jurisdictionRepository.findOne(jurisdictionId.get()) : new Jurisdiction();
  }

  @RequestMapping(value = "/jurisdiction/save", method = RequestMethod.POST)
  public String saveJurisdiction(@Valid Jurisdiction jurisdiction, BindingResult br, Model model, RedirectAttributes ra, @RequestAttribute User user) {
    if (br.hasErrors()) {
      model.addAttribute("jurisdictions", jurisdictionRepository.findAll(new Sort("name")));
      model.addAttribute("jurisdictionTypes", Arrays.asList(Jurisdiction.Type.values()));
      model.addAttribute("errors", getErrors(br));
      return "admin/jurisdictions";
    }

    jurisdictionRepository.save(jurisdiction);
    ra.addFlashAttribute("messages", getText("var.save", jurisdiction.getName()));
    return "redirect:/jurisdictions/list";
  }

}
