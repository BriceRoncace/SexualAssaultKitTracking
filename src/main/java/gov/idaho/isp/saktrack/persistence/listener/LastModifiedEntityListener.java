package gov.idaho.isp.saktrack.persistence.listener;

import gov.idaho.isp.saktrack.SexualAssaultKit;
import java.time.LocalDateTime;
import javax.persistence.PrePersist;
import javax.persistence.PreUpdate;
import org.springframework.security.core.context.SecurityContextHolder;

public class LastModifiedEntityListener {

  @PrePersist @PreUpdate
	public void initLastModifiedInformation(final SexualAssaultKitAware entity) {
		initLastModifiedInformation(entity.getSexualAssaultKit());
  }

  private void initLastModifiedInformation(SexualAssaultKit kit) {
    if (kit != null) {
      kit.setLastModified(LocalDateTime.now());
      kit.setLastModifiedBy(getCurrentUser());
    }
  }

  private String getCurrentUser() {
    if (SecurityContextHolder.getContext() != null && SecurityContextHolder.getContext().getAuthentication() != null) {
      return SecurityContextHolder.getContext().getAuthentication().getName();
    }
    return null;
  }
}