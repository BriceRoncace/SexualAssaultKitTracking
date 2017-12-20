package gov.idaho.isp.saktrack.service;

import gov.idaho.isp.saktrack.SexualAssaultKit;
import gov.idaho.isp.saktrack.audit.KitAudit;
import gov.idaho.isp.saktrack.user.User;

public interface AuditService {
  KitAudit auditKit(SexualAssaultKit modifiedKit, String notes, User user);
}
