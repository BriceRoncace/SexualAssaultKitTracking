package gov.idaho.isp.saktrack.service;

import gov.idaho.isp.saktrack.jurisdiction.Jurisdiction;
import gov.idaho.isp.saktrack.jurisdiction.JurisdictionRepository;
import gov.idaho.isp.saktrack.organization.Organization;
import gov.idaho.isp.saktrack.organization.OrganizationRepository;
import gov.idaho.isp.saktrack.persistence.search.SexualAssaultKitSearchCriteria;
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
      Jurisdiction j = jurisdictionRepository.findOne(criteria.getJurisdictionId());
      sj.add("Jurisdiction is " + j.getDisplayName());
    }

    if (criteria.getRequestingLeAgencyId() != null) {
      Organization org = organizationRepository.findOne(criteria.getRequestingLeAgencyId());
      sj.add("Requesting Agency is " + org.getName());
    }

    if (criteria.getCurrentJurisdictionId() != null) {
      Jurisdiction j = jurisdictionRepository.findOne(criteria.getCurrentJurisdictionId());
      sj.add("Jurisdiction is " + j.getDisplayName());
    }

    if (criteria.getCurrentAgencyId() != null) {
      Organization org = organizationRepository.findOne(criteria.getCurrentAgencyId());
      sj.add("Organization is " + org.getName());
    }

    return sj.toString();
  }
}
