/* 
 * Copyright 2017 Idaho State Police.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package gov.idaho.isp.saktrack.domain;

import gov.idaho.isp.saktrack.domain.SexualAssaultKit;
import java.time.LocalDateTime;
import jakarta.persistence.PrePersist;
import jakarta.persistence.PreUpdate;
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