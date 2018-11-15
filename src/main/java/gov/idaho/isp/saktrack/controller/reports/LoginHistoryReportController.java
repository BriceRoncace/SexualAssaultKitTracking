package gov.idaho.isp.saktrack.controller.reports;

import gov.idaho.isp.saktrack.domain.organization.OrganizationRepository;
import gov.idaho.isp.saktrack.domain.user.UserLogin;
import gov.idaho.isp.saktrack.domain.user.UserLoginRepository;
import gov.idaho.isp.saktrack.domain.user.UserLoginSpec;
import gov.idaho.isp.saktrack.service.csv.CsvExportService;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.web.PageableDefault;
import org.springframework.http.HttpEntity;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;

@Controller
public class LoginHistoryReportController {
  private static final int DEFAULT_PAGE_SIZE = 20;

  private final OrganizationRepository organizationRepository;
  private final UserLoginRepository userLoginRepository;
  private final CsvExportService csvExportService;

  public LoginHistoryReportController(OrganizationRepository organizationRepository, UserLoginRepository userLoginRepository, CsvExportService csvExportService) {
    this.organizationRepository = organizationRepository;
    this.userLoginRepository = userLoginRepository;
    this.csvExportService = csvExportService;
  }

  @GetMapping("/report/loginHistory")
  public String userLoginHistoryReport(UserLoginSpec spec, @PageableDefault(size=DEFAULT_PAGE_SIZE, sort={"displayName", "username"}) Pageable pageable, Model m) {
    m.addAttribute("organizations", organizationRepository.findAll(Sort.by("name")));
    m.addAttribute("page", userLoginRepository.findAll(spec, pageable));
    m.addAttribute("spec", spec);
    return "admin/reports/login-history";
  }

  @GetMapping("/report/loginHistory/download")
  public HttpEntity<byte[]> downloadUserLoginHistory(UserLoginSpec spec, @PageableDefault(size=DEFAULT_PAGE_SIZE, sort={"displayName", "username"}) Pageable pageable) {
    Page<UserLogin> page = userLoginRepository.findAll(spec, pageable);
    if (page.getTotalElements() > 10000) {
      return null;
    }

    return csvExportService.exportUserLoginList(userLoginRepository.findAll(spec, Sort.by("displayName", "username"))).toHttpEntity();
  }
}