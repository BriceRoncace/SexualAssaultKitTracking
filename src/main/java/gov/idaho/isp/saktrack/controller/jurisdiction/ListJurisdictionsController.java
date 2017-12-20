package gov.idaho.isp.saktrack.controller.jurisdiction;

import gov.idaho.isp.saktrack.controller.BaseController;
import gov.idaho.isp.saktrack.jurisdiction.Jurisdiction;
import gov.idaho.isp.saktrack.jurisdiction.JurisdictionRepository;
import java.util.Arrays;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

@Controller
public class ListJurisdictionsController extends BaseController {
  private final JurisdictionRepository jurisdictionRepository;

  public ListJurisdictionsController(JurisdictionRepository jurisdictionRepository) {
    this.jurisdictionRepository = jurisdictionRepository;
  }

  @RequestMapping(value = "/jurisdictions/list", method = RequestMethod.GET)
  public String listJurisdictions(Model model) {
    model.addAttribute("jurisdictions", jurisdictionRepository.findAll(new Sort("name")));
    model.addAttribute("jurisdictionTypes", Arrays.asList(Jurisdiction.Type.values()));
    return "admin/jurisdictions";
  }

}
