package gov.idaho.isp.saktrack.domain;

import gov.idaho.isp.saktrack.domain.jurisdiction.Jurisdiction;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

public interface SexualAssaultKitRepositoryPlus {
  SexualAssaultKit findBySerialNumber(String serialNumber);

  Page<SexualAssaultKit> findUnusedByOrganization(Long orgId, Pageable pageable);
  Page<SexualAssaultKit> findIncomingByOrganization(Long orgId, Pageable pageable);
  Page<SexualAssaultKit> findUsedByOrganization(Long orgId, Pageable pageable);

  Page<SexualAssaultKit> findPendingSubmissionByOrganization(Long orgId, Pageable pageable);
  Page<SexualAssaultKit> findReceivedByOrganization(Long orgId, Pageable pageable);
  Page<SexualAssaultKit> findByInLabAndRequestedBy(Long orgId, Pageable pageable);
  Page<SexualAssaultKit> findAnalyzedByOrganization(Long orgId, Pageable pageable);
  Page<SexualAssaultKit> findUnsubmittableByOrganization(Long orgId, Pageable pageable);

  Page<SexualAssaultKit> findByNeedsLegalAttention(Long orgId, Jurisdiction jurisdiction, Pageable pageable);
}