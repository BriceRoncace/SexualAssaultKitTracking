/* 
 * Copyright 2017 Idaho State Police.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package gov.idaho.isp.saktrack.controller.reports;

import gov.idaho.isp.saktrack.controller.BaseController;
import gov.idaho.isp.saktrack.domain.SexualAssaultKit;
import gov.idaho.isp.saktrack.domain.SexualAssaultKitRepository;
import gov.idaho.isp.saktrack.domain.jurisdiction.JurisdictionRepository;
import gov.idaho.isp.saktrack.domain.organization.OrganizationRepository;
import gov.idaho.isp.saktrack.domain.organization.OrganizationType;
import gov.idaho.isp.saktrack.domain.search.SexualAssaultKitSearchCriteria;
import gov.idaho.isp.saktrack.domain.search.SexualAssaultKitSpec;
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
import org.springframework.web.bind.annotation.GetMapping;

@Controller
public class KitsExceedingStatutoryReportController extends BaseController {
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

  @GetMapping("/report/exceedingStatutoryRequirements")
  public String postReport(SexualAssaultKitSearchCriteria criteria, Optional<Boolean> forward, Model model) {
    if (!Boolean.TRUE.equals(forward.orElse(Boolean.FALSE))) {
      if (criteria.isEmpty()) {
        model.addAttribute("errors", getText("empty.criteria"));
      }
      else {
        model.addAttribute("report", buildReport(criteria));
      }
    }

    model.addAttribute("criteria", criteria);
    model.addAttribute("jurisdictions", jurisdictionRepository.findAll(Sort.by("name")));
    model.addAttribute("leOrganizations", organizationRepository.findByTypeOrderByNameAsc(OrganizationType.LAW_ENFORCEMENT));
    model.addAttribute("filterText", filterTextService.buildReportFilterText(criteria));
    return "/admin/reports/exceeding-statutory-requirements";
  }

  @GetMapping("/report/exceedingStatutoryRequirements/download")
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
