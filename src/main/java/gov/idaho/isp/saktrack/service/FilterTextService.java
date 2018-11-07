package gov.idaho.isp.saktrack.service;

import gov.idaho.isp.saktrack.domain.search.SexualAssaultKitSearchCriteria;

public interface FilterTextService {
  String buildReportFilterText(SexualAssaultKitSearchCriteria criteria);
}
