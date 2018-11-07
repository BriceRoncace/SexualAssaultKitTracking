package gov.idaho.isp.saktrack.service;

import gov.idaho.isp.saktrack.domain.SexualAssaultKit;
import gov.idaho.isp.saktrack.domain.audit.KitAudit;
import gov.idaho.isp.saktrack.domain.user.User;

public interface AuditService {
  KitAudit auditKit(SexualAssaultKit modifiedKit, String notes, User user);
}
