package gov.idaho.isp.saktrack.controller.reports;

import gov.idaho.isp.saktrack.SexualAssaultKit;
import gov.idaho.isp.saktrack.jurisdiction.JurisdictionRepository;
import gov.idaho.isp.saktrack.organization.OrganizationRepository;
import gov.idaho.isp.saktrack.organization.OrganizationType;
import gov.idaho.isp.saktrack.persistence.SexualAssaultKitRepository;
import gov.idaho.isp.saktrack.persistence.search.SexualAssaultKitSearchCriteria;
import gov.idaho.isp.saktrack.persistence.search.SexualAssaultKitSpec;
import gov.idaho.isp.saktrack.report.StatutoryRequirementReport;
import gov.idaho.isp.saktrack.service.FilterTextService;
import gov.idaho.isp.saktrack.service.csv.CsvExportService;
import gov.idaho.isp.saktrack.sort.SortByLeAgencyName;
import gov.idaho.isp.saktrack.sort.SortBySerialNumber;
import java.util.List;
import java.util.Optional;
import org.springframework.data.domain.Sort;
import org.springframework.http.HttpEntity;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

@Controller
public class KitsExceedingStatutoryReportController {
  private final SexualAssaultKitRepository sexualAssaultKitRepository;
  private final OrganizationRepository organizationRepository;
  private final FilterTextService filterTextService;
  private final JurisdictionRepository jurisdictionRepository;
  private final CsvExportService csvExportService;

  public KitsExceedingStatutoryReportController(SexualAssaultKitRepository sexualAssaultKitRepository, OrganizationRepository organizationRepository, FilterTextService filterTextService, JurisdictionRepository jurisdictionRepository, CsvExportService csvExportService) {
    this.sexualAssaultKitRepository = sexualAssaultKitRepository;
    this.organizationRepository = organizationRepository;
    this.filterTextService = filterTextService;
    this.jurisdictionRepository = jurisdictionRepository;
    this.csvExportService = csvExportService;
  }

  @RequestMapping(value = "/report/exceedingStatutoryRequirements", method = RequestMethod.GET)
  public String postReport(SexualAssaultKitSearchCriteria criteria, Optional<Boolean> forward, Model model) {
    if (!Boolean.TRUE.equals(forward.orElse(Boolean.FALSE))) {
      model.addAttribute("report", buildReport(criteria));
    }

    model.addAttribute("criteria", criteria);
    model.addAttribute("jurisdictions", jurisdictionRepository.findAll(new Sort("name")));
    model.addAttribute("leOrganizations", organizationRepository.findByTypeOrderByNameAsc(OrganizationType.LAW_ENFORCEMENT));
    model.addAttribute("filterText", filterTextService.buildReportFilterText(criteria));
    return "/admin/reports/exceeding-statutory-requirements";
  }

  @RequestMapping(value = "/report/exceedingStatutoryRequirements/download", method = RequestMethod.GET)
  public HttpEntity<byte[]> downloadReport(SexualAssaultKitSearchCriteria criteria) {
    return csvExportService.exportExceedingStatutoryRequirementsReport(buildReport(criteria)).toHttpEntity();
  }

  private StatutoryRequirementReport buildReport(SexualAssaultKitSearchCriteria criteria) {
    criteria.setRequestingLeAgencyNotNull(Boolean.TRUE);
    criteria.setHasCollectedDate(Boolean.TRUE);
    criteria.setCompletedDateIsNull(Boolean.TRUE);

    List<SexualAssaultKit> kits = sexualAssaultKitRepository.findAll(new SexualAssaultKitSpec(criteria));
    kits.sort(new SortBySerialNumber());
    kits.sort(new SortByLeAgencyName());

    return new StatutoryRequirementReport(kits);
  }
}
