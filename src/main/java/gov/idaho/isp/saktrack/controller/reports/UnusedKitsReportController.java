package gov.idaho.isp.saktrack.controller.reports;

import gov.idaho.isp.saktrack.SexualAssaultKit;
import gov.idaho.isp.saktrack.jurisdiction.JurisdictionRepository;
import gov.idaho.isp.saktrack.organization.OrganizationRepository;
import gov.idaho.isp.saktrack.persistence.SexualAssaultKitRepository;
import gov.idaho.isp.saktrack.persistence.search.SexualAssaultKitSearchCriteria;
import gov.idaho.isp.saktrack.persistence.search.SexualAssaultKitSpec;
import gov.idaho.isp.saktrack.report.CurrentAssignmentReport;
import gov.idaho.isp.saktrack.service.FilterTextService;
import gov.idaho.isp.saktrack.service.csv.CsvExportService;
import gov.idaho.isp.saktrack.sort.SortByAssignedAgencyName;
import gov.idaho.isp.saktrack.sort.SortByExpirationDate;
import java.util.List;
import java.util.Optional;
import org.springframework.data.domain.Sort;
import org.springframework.http.HttpEntity;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

@Controller
public class UnusedKitsReportController {
  private final SexualAssaultKitRepository sexualAssaultKitRepository;
  private final OrganizationRepository organizationRepository;
  private final FilterTextService filterTextService;
  private final JurisdictionRepository jurisdictionRepository;
  private final CsvExportService csvExportService;

  public UnusedKitsReportController(SexualAssaultKitRepository sexualAssaultKitRepository, OrganizationRepository organizationRepository, FilterTextService filterTextService, JurisdictionRepository jurisdictionRepository, CsvExportService csvExportService) {
    this.sexualAssaultKitRepository = sexualAssaultKitRepository;
    this.organizationRepository = organizationRepository;
    this.filterTextService = filterTextService;
    this.jurisdictionRepository = jurisdictionRepository;
    this.csvExportService = csvExportService;
  }

  @RequestMapping(value = "/report/unusedKits", method = RequestMethod.GET)
  public String postReport(SexualAssaultKitSearchCriteria criteria, Optional<Boolean> forward, Model model) {
    if (!Boolean.TRUE.equals(forward.orElse(Boolean.FALSE))) {
      model.addAttribute("report", buildReport(criteria));
    }

    model.addAttribute("criteria", criteria);
    model.addAttribute("jurisdictions", jurisdictionRepository.findAll(new Sort("name")));
    model.addAttribute("organizations", organizationRepository.findAll(new Sort("name")));
    model.addAttribute("filterText", filterTextService.buildReportFilterText(criteria));
    return "/admin/reports/unused-kits";
  }

  @RequestMapping(value = "/report/unusedKits/download", method = RequestMethod.GET)
  public HttpEntity<byte[]> downloadReport(SexualAssaultKitSearchCriteria criteria) {
    return csvExportService.exportUnusedKitsReport(buildReport(criteria)).toHttpEntity();
  }

  private CurrentAssignmentReport buildReport(SexualAssaultKitSearchCriteria criteria) {
    criteria.setHasCollectedDate(Boolean.FALSE);

    List<SexualAssaultKit> kits = sexualAssaultKitRepository.findAll(new SexualAssaultKitSpec(criteria));
    kits.sort(new SortByExpirationDate());
    kits.sort(new SortByAssignedAgencyName());

    return new CurrentAssignmentReport(kits);
  }
}
