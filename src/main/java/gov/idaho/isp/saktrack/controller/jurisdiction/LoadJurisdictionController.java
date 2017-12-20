package gov.idaho.isp.saktrack.controller.jurisdiction;

import gov.idaho.isp.saktrack.controller.BaseController;
import gov.idaho.isp.saktrack.jurisdiction.Jurisdiction;
import gov.idaho.isp.saktrack.jurisdiction.JurisdictionRepository;
import java.util.Arrays;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

@Controller
public class LoadJurisdictionController extends BaseController {
  private final JurisdictionRepository jurisdictionRepository;

  public LoadJurisdictionController(JurisdictionRepository jurisdictionRepository) {
    this.jurisdictionRepository = jurisdictionRepository;
  }

  @RequestMapping(value = "/jurisdiction/{id}", method = RequestMethod.GET)
  public String loadJurisdiction(@PathVariable Long id, Model model) {
    if (id != null) {
      model.addAttribute("jurisdiction", jurisdictionRepository.findOne(id));
    }

    model.addAttribute("jurisdictions", jurisdictionRepository.findAll(new Sort("name")));
    model.addAttribute("jurisdictionTypes", Arrays.asList(Jurisdiction.Type.values()));
    return "admin/jurisdictions";
  }
}
