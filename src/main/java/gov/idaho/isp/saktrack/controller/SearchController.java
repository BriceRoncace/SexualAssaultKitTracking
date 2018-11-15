package gov.idaho.isp.saktrack.controller;

import gov.idaho.isp.saktrack.domain.ChainOfCustodyEvent;
import gov.idaho.isp.saktrack.domain.LawEnforcementDetails.NonSubmissionReason;
import gov.idaho.isp.saktrack.domain.MedicalDetails;
import gov.idaho.isp.saktrack.domain.SexualAssaultKit;
import gov.idaho.isp.saktrack.domain.SexualAssaultKitRepository;
import gov.idaho.isp.saktrack.domain.jurisdiction.JurisdictionRepository;
import gov.idaho.isp.saktrack.domain.organization.Organization;
import gov.idaho.isp.saktrack.domain.organization.OrganizationRepository;
import gov.idaho.isp.saktrack.domain.organization.OrganizationType;
import gov.idaho.isp.saktrack.domain.search.SexualAssaultKitSearchCriteria;
import gov.idaho.isp.saktrack.domain.search.SexualAssaultKitSpec;
import gov.idaho.isp.saktrack.domain.user.AdminUser;
import gov.idaho.isp.saktrack.domain.user.User;
import gov.idaho.isp.saktrack.domain.user.organization.OrganizationUser;
import gov.idaho.isp.saktrack.service.csv.CsvExportService;
import java.util.List;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.web.PageableDefault;
import org.springframework.http.HttpEntity;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestAttribute;

@Controller
public class SearchController {
  private final SexualAssaultKitRepository sexualAssaultKitRepository;
  private final OrganizationRepository organizationRepository;
  private final JurisdictionRepository jurisdictionRepository;
  private final CsvExportService csvExportService;

  public SearchController(SexualAssaultKitRepository sexualAssaultKitRepository, OrganizationRepository organizationRepository, JurisdictionRepository jurisdictionRepository, CsvExportService csvExportService) {
    this.sexualAssaultKitRepository = sexualAssaultKitRepository;
    this.organizationRepository = organizationRepository;
    this.jurisdictionRepository = jurisdictionRepository;
    this.csvExportService = csvExportService;
  }

  @GetMapping("/admin/search")
  public String adminSearch(SexualAssaultKitSearchCriteria criteria, @PageableDefault(size=25, sort = "lastModified", direction = Sort.Direction.DESC) Pageable pageable, Model model) {
    Page<SexualAssaultKit> page = sexualAssaultKitRepository.findAll(new SexualAssaultKitSpec(criteria), pageable);
    model.addAttribute("criteria", criteria);
    model.addAttribute("page", page);
    model.addAttribute("organizations", organizationRepository.findAll(Sort.by("name")));
    setCommonModelAttributes(model);
    return "/admin/search";
  }

  @GetMapping("/admin/search/download")
  public HttpEntity<byte[]> adminSearchDownload(SexualAssaultKitSearchCriteria criteria) {
    return csvExportService.exportKitSearchResults(sexualAssaultKitRepository.findAll(new SexualAssaultKitSpec(criteria))).toHttpEntity();
  }

  @GetMapping("/search")
  public String search(@RequestAttribute User user, SexualAssaultKitSearchCriteria criteria, @PageableDefault(size=50, sort = "lastModified", direction = Sort.Direction.DESC) Pageable pageable, Model model) {
    OrganizationUser orgUser;
    if (user instanceof AdminUser) {
      return "redirect:/admin/search";
    }
    else {
      orgUser = (OrganizationUser) user;
    }

    criteria = setCriteriaToFilterForAgencyAndOrJurisdiction(orgUser, criteria);
    Page<SexualAssaultKit> page = sexualAssaultKitRepository.findAll(new SexualAssaultKitSpec(criteria), pageable);
    model.addAttribute("criteria", criteria);
    model.addAttribute("page", page);
    model.addAttribute("userOrg", orgUser.getOrganization());
    model.addAttribute("organizations", createFilteredOrgsIfProsecutor(orgUser));
    setCommonModelAttributes(model);
    return "/org-users/organization-search";
  }

  @GetMapping("/search/download")
  public HttpEntity<byte[]> searchDownload(@RequestAttribute User user, SexualAssaultKitSearchCriteria criteria) {
    if (!(user instanceof AdminUser)) {
      OrganizationUser orgUser = (OrganizationUser) user;
      if (!criteria.isEmpty()) {
        criteria = setCriteriaToFilterForAgencyAndOrJurisdiction(orgUser, criteria);
      }
      return csvExportService.exportKitSearchResults(sexualAssaultKitRepository.findAll(new SexualAssaultKitSpec(criteria)), orgUser.getOrganization().getType()).toHttpEntity();
    }

    return null;
  }

  private void setCommonModelAttributes(Model model) {
    model.addAttribute("leOrganizations", organizationRepository.findByTypeOrderByNameAsc(OrganizationType.LAW_ENFORCEMENT));
    model.addAttribute("eventTypes", ChainOfCustodyEvent.EventType.values());
    model.addAttribute("victimTypes", MedicalDetails.VictimType.values());
    model.addAttribute("nonSubmissionReasons", NonSubmissionReason.values());
    model.addAttribute("jurisdictions", jurisdictionRepository.findAll(Sort.by("name")));
  }

  private SexualAssaultKitSearchCriteria setCriteriaToFilterForAgencyAndOrJurisdiction(OrganizationUser user, SexualAssaultKitSearchCriteria criteria) {
    if (OrganizationType.LEGAL == user.getOrganization().getType()) {
      criteria.setJurisdictionId(user.getOrganization().getJurisdiction().getId());
      criteria.setReviewingProsecutorOrganization(user.getOrganization().getId());
    }
    else {
      criteria.setJurisdictionId(null);
      criteria.setEventOrganization(user.getOrganization().getId());
    }
    return criteria;
  }

  private List<Organization> createFilteredOrgsIfProsecutor(OrganizationUser user) {
    if (OrganizationType.LEGAL == user.getOrganization().getType()) {
      return organizationRepository.findByJurisdictionOrderByNameAsc(user.getOrganization().getJurisdiction());
    }
    return organizationRepository.findAll(Sort.by("name"));
  }
}
