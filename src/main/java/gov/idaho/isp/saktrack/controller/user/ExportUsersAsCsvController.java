package gov.idaho.isp.saktrack.controller.user;

import gov.idaho.isp.saktrack.service.csv.CsvExportService;
import gov.idaho.isp.saktrack.user.persistence.AbstractUserDirectory;
import gov.idaho.isp.saktrack.user.persistence.AbstractUserSpec;
import org.springframework.http.HttpEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;

@Controller
public class ExportUsersAsCsvController {
  private final AbstractUserDirectory abstractUserDirectory;
  private final CsvExportService csvExportService;

  public ExportUsersAsCsvController(AbstractUserDirectory abstractUserDirectory, CsvExportService csvExportService) {
    this.abstractUserDirectory = abstractUserDirectory;
    this.csvExportService = csvExportService;
  }

  @GetMapping(value = "/users/download")
  public HttpEntity<byte[]> downloadUsers(AbstractUserSpec spec) {
    return csvExportService.exportUserList(abstractUserDirectory.findAll(spec)).toHttpEntity();
  }
}
