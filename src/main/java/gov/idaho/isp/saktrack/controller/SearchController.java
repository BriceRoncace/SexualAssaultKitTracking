package gov.idaho.isp.saktrack.controller;

import gov.idaho.isp.saktrack.ChainOfCustodyEvent;
import gov.idaho.isp.saktrack.LawEnforcementDetails.NonSubmissionReason;
import gov.idaho.isp.saktrack.MedicalDetails;
import gov.idaho.isp.saktrack.SexualAssaultKit;
import gov.idaho.isp.saktrack.jurisdiction.JurisdictionRepository;
import gov.idaho.isp.saktrack.organization.Organization;
import gov.idaho.isp.saktrack.organization.OrganizationRepository;
import gov.idaho.isp.saktrack.organization.OrganizationType;
import gov.idaho.isp.saktrack.persistence.SexualAssaultKitRepository;
import gov.idaho.isp.saktrack.persistence.search.SexualAssaultKitSearchCriteria;
import gov.idaho.isp.saktrack.persistence.search.SexualAssaultKitSpec;
import gov.idaho.isp.saktrack.service.csv.CsvExportService;
import gov.idaho.isp.saktrack.user.AdminUser;
import gov.idaho.isp.saktrack.user.User;
import gov.idaho.isp.saktrack.user.organization.OrganizationUser;
import java.util.List;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.web.PageableDefault;
import org.springframework.http.HttpEntity;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestAttribute;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

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

  @RequestMapping(value = "/admin/search", method = RequestMethod.GET)
  public String adminSearch(SexualAssaultKitSearchCriteria criteria, @PageableDefault(size=25, sort = "lastModified", direction = Sort.Direction.DESC) Pageable pageable, Model model) {
    if (!criteria.isEmpty()) {
      Page<SexualAssaultKit> page = sexualAssaultKitRepository.findAll(new SexualAssaultKitSpec(criteria), pageable);
      model.addAttribute("criteria", criteria);
      model.addAttribute("page", page);
    }
    model.addAttribute("organizations", organizationRepository.findAll(new Sort("name")));
    setCommonModelAttributes(model);
    return "/admin/search";
  }

  @RequestMapping(value = "/admin/search/download", method = RequestMethod.GET)
  public HttpEntity<byte[]> adminSearchDownload(SexualAssaultKitSearchCriteria criteria) {
    return csvExportService.exportKitSearchResults(sexualAssaultKitRepository.findAll(new SexualAssaultKitSpec(criteria))).toHttpEntity();
  }

  @RequestMapping(value = "/search", method = RequestMethod.GET)
  public String search(@RequestAttribute User user, SexualAssaultKitSearchCriteria criteria, @PageableDefault(size=50, sort = "lastModified", direction = Sort.Direction.DESC) Pageable pageable, Model model) {
    OrganizationUser orgUser;
    if (user instanceof AdminUser) {
      return "redirect:/admin/search";
    }
    else {
      orgUser = (OrganizationUser) user;
    }

    if (!criteria.isEmpty()) {
      criteria = setCriteriaToFilterForAgencyAndOrJurisdiction(orgUser, criteria);
      Page<SexualAssaultKit> page = sexualAssaultKitRepository.findAll(new SexualAssaultKitSpec(criteria), pageable);
      model.addAttribute("criteria", criteria);
      model.addAttribute("page", page);
    }
    model.addAttribute("userOrg", orgUser.getOrganization());
    model.addAttribute("organizations", createFilteredOrgsIfProsecutor(orgUser));
    setCommonModelAttributes(model);
    return "/org-users/organization-search";
  }

  @RequestMapping(value = "/search/download", method = RequestMethod.GET)
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
    model.addAttribute("jurisdictions", jurisdictionRepository.findAll(new Sort("name")));
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
    return organizationRepository.findAll(new Sort("name"));
  }
}
