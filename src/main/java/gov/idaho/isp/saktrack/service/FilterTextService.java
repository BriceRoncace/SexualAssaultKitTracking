package gov.idaho.isp.saktrack.service;

import gov.idaho.isp.saktrack.persistence.search.SexualAssaultKitSearchCriteria;

public interface FilterTextService {
  String buildReportFilterText(SexualAssaultKitSearchCriteria criteria);
}
