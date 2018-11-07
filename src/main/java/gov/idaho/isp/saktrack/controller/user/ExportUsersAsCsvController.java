package gov.idaho.isp.saktrack.controller.user;

import gov.idaho.isp.saktrack.domain.user.AbstractUserRepository;
import gov.idaho.isp.saktrack.domain.user.AbstractUserSpec;
import gov.idaho.isp.saktrack.service.csv.CsvExportService;
import org.springframework.http.HttpEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;

@Controller
public class ExportUsersAsCsvController {
  private final AbstractUserRepository abstractUserRepository;
  private final CsvExportService csvExportService;

  public ExportUsersAsCsvController(AbstractUserRepository abstractUserRepository, CsvExportService csvExportService) {
    this.abstractUserRepository = abstractUserRepository;
    this.csvExportService = csvExportService;
  }

  @GetMapping("/users/download")
  public HttpEntity<byte[]> downloadUsers(AbstractUserSpec spec) {
    return csvExportService.exportUserList(abstractUserRepository.findAll(spec)).toHttpEntity();
  }
}
