package gov.idaho.isp.saktrack.controller.admin;

import gov.idaho.isp.saktrack.domain.SexualAssaultKitRepository;
import gov.idaho.isp.saktrack.domain.jurisdiction.JurisdictionRepository;
import gov.idaho.isp.saktrack.domain.organization.OrganizationRepository;
import gov.idaho.isp.saktrack.domain.organization.OrganizationSearchCriteria;
import gov.idaho.isp.saktrack.domain.organization.OrganizationSpec;
import gov.idaho.isp.saktrack.domain.organization.OrganizationType;
import gov.idaho.isp.saktrack.domain.user.AdminUser;
import gov.idaho.isp.saktrack.domain.user.User;
import gov.idaho.isp.saktrack.util.CookieUtils;
import gov.idaho.isp.saktrack.util.PagingUtils;
import gov.idaho.isp.saktrack.util.UserUtils;
import java.io.UnsupportedEncodingException;
import java.util.Arrays;
import java.util.Map;
import java.util.Optional;
import javax.servlet.http.HttpServletResponse;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.web.PageableDefault;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.CookieValue;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.RequestAttribute;
import org.springframework.web.bind.annotation.RequestParam;

@Controller
public class AdminDashboardController {
  private final OrganizationRepository organizationRepository;
  private final SexualAssaultKitRepository sexualAssaultKitRepository;
  private final JurisdictionRepository jurisdictionRepository;

  private static final String ORG_FILTER_COOKIE_NAME = "adminDashboardOrgFilter";

  public AdminDashboardController(OrganizationRepository organizationRepository, SexualAssaultKitRepository sexualAssaultKitRepository, JurisdictionRepository jurisdictionRepository) {
    this.organizationRepository = organizationRepository;
    this.sexualAssaultKitRepository = sexualAssaultKitRepository;
    this.jurisdictionRepository = jurisdictionRepository;
  }

  @ModelAttribute
  public AdminUser prepareLoggedInUser(@RequestAttribute User user) {
    UserUtils.verifyUserTypeOrThrowException(user, User.Type.ADMIN);
    return (AdminUser) user;
  }

  @GetMapping("/admin/dashboard")
  public String dashboard(@CookieValue Optional<String> adminDashboardOrgFilter,
                          @PageableDefault(size=15, sort = "name", direction = Sort.Direction.ASC) Pageable pageable,
                          OrganizationSearchCriteria criteria,
                          @RequestParam Map<String,String> requestParams,
                          HttpServletResponse res,
                          Model model) throws UnsupportedEncodingException {

    if (requestParams.isEmpty() && adminDashboardOrgFilter.isPresent()) {
      return "redirect:/admin/dashboard" + adminDashboardOrgFilter.get();
    }

    Page page = doSearch(criteria, pageable);
    model.addAttribute("page", page);
    model.addAttribute("criteria", criteria);
    model.addAttribute("kitsMissingEvents", sexualAssaultKitRepository.findByQuestionableEventsTrue());
    model.addAttribute("orgTypes", Arrays.asList(OrganizationType.values()));
    model.addAttribute("jurisdictions", jurisdictionRepository.findAll(new Sort("name")));

    res.addCookie(CookieUtils.createEncoded(ORG_FILTER_COOKIE_NAME, criteria.buildUrlParams(page)));
    return "admin/dashboard";
  }

  private Page doSearch(OrganizationSearchCriteria criteria, Pageable pageable) {
    if (!criteria.isEmpty()) {
      return PagingUtils.getFirstPageIfRequestedIsBlank(pr -> organizationRepository.findAll(new OrganizationSpec(criteria), pr), pageable);
    }
    return PagingUtils.getFirstPageIfRequestedIsBlank(pr -> organizationRepository.findAll(pr), pageable);
  }
}