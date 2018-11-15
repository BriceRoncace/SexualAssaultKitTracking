package gov.idaho.isp.saktrack.controller.reports;

import gov.idaho.isp.saktrack.domain.SexualAssaultKit;
import gov.idaho.isp.saktrack.domain.jurisdiction.JurisdictionRepository;
import gov.idaho.isp.saktrack.domain.organization.OrganizationRepository;
import gov.idaho.isp.saktrack.domain.SexualAssaultKitRepository;
import gov.idaho.isp.saktrack.domain.search.SexualAssaultKitSearchCriteria;
import gov.idaho.isp.saktrack.report.CurrentAssignmentReport;
import gov.idaho.isp.saktrack.service.csv.CsvExportService;
import gov.idaho.isp.saktrack.sort.SortByAssignedAgencyName;
import gov.idaho.isp.saktrack.sort.SortBySerialNumber;
import java.util.List;
import org.springframework.data.domain.Sort;
import org.springframework.http.HttpEntity;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;

@Controller
public class PotentialCocErrorsReportController {
  private final SexualAssaultKitRepository sexualAssaultKitRepository;
  private final OrganizationRepository organizationRepository;
  private final JurisdictionRepository jurisdictionRepository;
  private final CsvExportService csvExportService;

  public PotentialCocErrorsReportController(SexualAssaultKitRepository sexualAssaultKitRepository, OrganizationRepository organizationRepository, JurisdictionRepository jurisdictionRepository, CsvExportService csvExportService) {
    this.sexualAssaultKitRepository = sexualAssaultKitRepository;
    this.organizationRepository = organizationRepository;
    this.jurisdictionRepository = jurisdictionRepository;
    this.csvExportService = csvExportService;
  }

  @GetMapping("/report/cocErrors")
  public String getReport(Model model) {
    model.addAttribute("report", buildReport());
    model.addAttribute("jurisdictions", jurisdictionRepository.findAll(Sort.by("name")));
    model.addAttribute("organizations", organizationRepository.findAll());
    return "/admin/reports/coc-errors";
  }

  @GetMapping("/report/cocErrors/download")
  public HttpEntity<byte[]> downloadReport(SexualAssaultKitSearchCriteria criteria) {
    return csvExportService.exportKitsWithQuestionableEventsReport(buildReport()).toHttpEntity();
  }

  private CurrentAssignmentReport buildReport() {
    List<SexualAssaultKit> kits = sexualAssaultKitRepository.findByQuestionableEventsTrue();
    kits.sort(new SortBySerialNumber());
    kits.sort(new SortByAssignedAgencyName());
    return new CurrentAssignmentReport(kits);
  }
}
