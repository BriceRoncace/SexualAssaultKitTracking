package gov.idaho.isp.saktrack.controller.admin;

import gov.idaho.isp.saktrack.domain.jurisdiction.JurisdictionRepository;
import gov.idaho.isp.saktrack.exception.SexualAssaultKitTrackingException;
import gov.idaho.isp.saktrack.service.DataMigration;
import java.util.function.Function;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

@Controller
public class DataImportController {
  private final DataMigration dataMigration;
  private final JurisdictionRepository jurisdictionRepository;

  public DataImportController(DataMigration dataMigration, JurisdictionRepository jurisdictionRepository) {
    this.dataMigration = dataMigration;
    this.jurisdictionRepository = jurisdictionRepository;
  }

  @GetMapping("/dataMigration")
  public String dataMigration(Model model) {
    model.addAttribute("jurisdictions", jurisdictionRepository.findAll(Sort.by("name")));
    return "admin/data-migration";
  }

  @PostMapping("/orgImport")
  public String orgImport(MultipartFile file, RedirectAttributes ra) {
    return processFile(file, ra, f -> dataMigration.importNewOrganizations(f));
  }

  @PostMapping("/userImport")
  public String userImport(MultipartFile file, RedirectAttributes ra) {
    return processFile(file, ra, f -> dataMigration.importNewOrganizationUsers(f));
  }

  private String processFile(MultipartFile file, RedirectAttributes ra, Function<MultipartFile, Integer> function) {
    if (!file.isEmpty()) {
      try {
        Integer records = function.apply(file);
        ra.addFlashAttribute("messages", String.format("%s records added.", records));
      }
      catch (SexualAssaultKitTrackingException ex) {
        ra.addFlashAttribute("errors", ex.getErrors());
      }
    }
    return "redirect:/dataMigration";
  }
}
