package gov.idaho.isp.saktrack.service;

import gov.idaho.isp.saktrack.domain.jurisdiction.Jurisdiction;
import gov.idaho.isp.saktrack.domain.jurisdiction.JurisdictionRepository;
import gov.idaho.isp.saktrack.domain.organization.Organization;
import gov.idaho.isp.saktrack.domain.organization.OrganizationRepository;
import gov.idaho.isp.saktrack.domain.search.SexualAssaultKitSearchCriteria;
import java.util.StringJoiner;
import org.springframework.stereotype.Service;

@Service
public class FilterTextServiceImpl implements FilterTextService {
  private final OrganizationRepository organizationRepository;
  private final JurisdictionRepository jurisdictionRepository;

  public FilterTextServiceImpl(OrganizationRepository organizationRepository, JurisdictionRepository jurisdictionRepository) {
    this.organizationRepository = organizationRepository;
    this.jurisdictionRepository = jurisdictionRepository;
  }

  @Override
  public String buildReportFilterText(SexualAssaultKitSearchCriteria criteria) {
    StringJoiner sj = new StringJoiner(" | ");

    if (criteria.getCollectedDate() != null) {
      String dateFilterText = criteria.getCollectedDate().getFilterText("Collected Date");
      if (dateFilterText != null) {
        sj.add(dateFilterText);
      }
    }

    if (criteria.getJurisdictionId() != null) {
      Jurisdiction j = jurisdictionRepository.findById(criteria.getJurisdictionId()).orElse(null);
      sj.add("Jurisdiction is " + j.getDisplayName());
    }

    if (criteria.getRequestingLeAgencyId() != null) {
      Organization org = organizationRepository.findById(criteria.getRequestingLeAgencyId()).orElse(null);
      sj.add("Requesting Agency is " + org.getName());
    }

    if (criteria.getCurrentJurisdictionId() != null) {
      Jurisdiction j = jurisdictionRepository.findById(criteria.getCurrentJurisdictionId()).orElse(null);
      sj.add("Jurisdiction is " + j.getDisplayName());
    }

    if (criteria.getCurrentAgencyId() != null) {
      Organization org = organizationRepository.findById(criteria.getCurrentAgencyId()).orElse(null);
      sj.add("Organization is " + org.getName());
    }

    return sj.toString();
  }
}
