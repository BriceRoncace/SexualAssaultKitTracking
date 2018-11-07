package gov.idaho.isp.saktrack.service;

import gov.idaho.isp.saktrack.domain.user.dto.AdminUserForm;
import gov.idaho.isp.saktrack.domain.user.dto.OrgUserForm;
import java.util.Optional;

public interface UserFormFactory {
  OrgUserForm getOrgUserForm(Optional<Long> userId, Long organizationId, boolean hasCurrentPassword);
  OrgUserForm getOrgUserForm(Optional<Long> organizationId);

  AdminUserForm getAdminUserForm(Optional<Long> userId, boolean hasCurrentPassword);
}
