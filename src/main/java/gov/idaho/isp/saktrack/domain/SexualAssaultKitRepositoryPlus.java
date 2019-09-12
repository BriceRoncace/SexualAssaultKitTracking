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