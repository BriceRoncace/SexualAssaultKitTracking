package gov.idaho.isp.saktrack.controller.reports;

import gov.idaho.isp.saktrack.LawEnforcementDetails.NonSubmissionReason;
import gov.idaho.isp.saktrack.SexualAssaultKit;
import gov.idaho.isp.saktrack.persistence.SexualAssaultKitRepository;
import gov.idaho.isp.saktrack.report.CurrentAssignmentReport;
import gov.idaho.isp.saktrack.report.RequestingAgencyReport;
import gov.idaho.isp.saktrack.service.csv.CsvExportService;
import java.time.LocalDate;
import java.util.List;
import java.util.stream.Collectors;
import org.springframework.http.HttpEntity;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

@Controller
public class EvidenceCollectionReport {
  private final SexualAssaultKitRepository sexualAssaultKitRepository;
  private final CsvExportService csvExportService;

  public EvidenceCollectionReport(SexualAssaultKitRepository sexualAssaultKitRepository, CsvExportService csvExportService) {
    this.sexualAssaultKitRepository = sexualAssaultKitRepository;
    this.csvExportService = csvExportService;
  }

  @RequestMapping(value = "/report/timeframe", method = RequestMethod.GET)
  public String loadTimeframeReport() {
    return "/admin/reports/timeframe";
  }

  @RequestMapping(value = "/report/timeframe", method = RequestMethod.POST)
  public String getTimeframeReport(@RequestParam LocalDate start, @RequestParam LocalDate end, Model model) {
    model.addAttribute("startDate", start);
    model.addAttribute("endDate", end);

    model.addAttribute("distributedKitsReport", buildDistributedKitsReport(start, end));
    model.addAttribute("receivedFromMedicalReport", buildReceivedCollectedKitsReport(start, end));

    List<SexualAssaultKit> unsubmittableKits = sexualAssaultKitRepository.findUnsubmittableInDateRange(start, end);
    List<SexualAssaultKit> noEvidence = unsubmittableKits.stream().filter(k -> k.getLeDetails().getNonSubmissionReason() == NonSubmissionReason.NO_EVIDENCE).collect(Collectors.toList());
    List<SexualAssaultKit> notACrime = unsubmittableKits.stream().filter(k -> k.getLeDetails().getNonSubmissionReason() == NonSubmissionReason.NOT_A_CRIME).collect(Collectors.toList());
    List<SexualAssaultKit> noTesting = unsubmittableKits.stream().filter(k -> k.getLeDetails().getNonSubmissionReason() == NonSubmissionReason.NO_TESTING).collect(Collectors.toList());
    model.addAttribute("unsubmittableKitsSize", unsubmittableKits.size());
    model.addAttribute("noEvidenceReport", new RequestingAgencyReport(noEvidence));
    model.addAttribute("notACrimeReport", new RequestingAgencyReport(notACrime));
    model.addAttribute("noTestingReport", new RequestingAgencyReport(noTesting));

    model.addAttribute("submittedKitsReport", buildSubmittedKitsReport(start, end));
    model.addAttribute("databaseKitsReport", buildDatabaseKitsReport(start, end));
    model.addAttribute("hitsInKitsReport", buildHitsInKitsReport(start, end));

    return "/admin/reports/timeframe";
  }

  @RequestMapping(value = "/report/distributedKitsReport/download", method = RequestMethod.GET)
  public HttpEntity<byte[]> downloadDistributedKitsReport(@RequestParam LocalDate start, @RequestParam LocalDate end) {
    return csvExportService.exportUnusedKitsReport(buildDistributedKitsReport(start, end)).toHttpEntity();
  }

  @RequestMapping(value = "/report/receivedCollectedKitsReport/download", method = RequestMethod.GET)
  public HttpEntity<byte[]> downloadReceivedCollectedKitsReport(@RequestParam LocalDate start, @RequestParam LocalDate end) {
    return csvExportService.exportReceivedCollectedKitsReport(buildReceivedCollectedKitsReport(start, end)).toHttpEntity();
  }

  @RequestMapping(value = "/report/unsubmittableKitsReport/download", method = RequestMethod.GET)
  public HttpEntity<byte[]> downloadUnsubmittableKitsReport(@RequestParam LocalDate start, @RequestParam LocalDate end) {
    return csvExportService.exportUnsubmittableKitsReport(new RequestingAgencyReport(sexualAssaultKitRepository.findUnsubmittableInDateRange(start, end))).toHttpEntity();
  }

  @RequestMapping(value = "/report/submittedKitsReport/download", method = RequestMethod.GET)
  public HttpEntity<byte[]> downloadSubmittedKitsReport(@RequestParam LocalDate start, @RequestParam LocalDate end) {
    return csvExportService.exportSubmittedKitsReport(buildSubmittedKitsReport(start, end)).toHttpEntity();
  }

  @RequestMapping(value = "/report/databaseKitsReport/download", method = RequestMethod.GET)
  public HttpEntity<byte[]> downloadDatabaseKitsReport(@RequestParam LocalDate start, @RequestParam LocalDate end) {
    return csvExportService.exportDatabaseKitsReport(buildDatabaseKitsReport(start, end)).toHttpEntity();
  }

  @RequestMapping(value = "/report/hitsInKitsReport/download", method = RequestMethod.GET)
  public HttpEntity<byte[]> downloadHitsInKitsReport(@RequestParam LocalDate start, @RequestParam LocalDate end) {
    return csvExportService.exportDatabaseHitKitsReport(buildHitsInKitsReport(start, end)).toHttpEntity();
  }

  private CurrentAssignmentReport buildDistributedKitsReport(LocalDate start, LocalDate end) {
    return new CurrentAssignmentReport(sexualAssaultKitRepository.findDistributedInDateRange(start, end));
  }

  private RequestingAgencyReport buildReceivedCollectedKitsReport(LocalDate start, LocalDate end) {
    return new RequestingAgencyReport(sexualAssaultKitRepository.findReceivedByLeInDateRange(start, end));
  }

  private RequestingAgencyReport buildSubmittedKitsReport(LocalDate start, LocalDate end) {
   return new RequestingAgencyReport(sexualAssaultKitRepository.findSubmittedByDateRange(start, end));
  }

  private RequestingAgencyReport buildDatabaseKitsReport(LocalDate start, LocalDate end) {
    return new RequestingAgencyReport(sexualAssaultKitRepository.findEnteredInDatabaseByDateRange(start, end));
  }

  private RequestingAgencyReport buildHitsInKitsReport(LocalDate start, LocalDate end) {
    return new RequestingAgencyReport(sexualAssaultKitRepository.findHitsInKitsByDateRange(start, end));
  }
}
