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

package gov.idaho.isp.saktrack.domain.user.organization;

import java.util.List;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

public interface OrganizationUserRepository extends JpaRepository<AbstractOrganizationUser,Long> {
  AbstractOrganizationUser findByUsernameIgnoreCase(String username);
  List<AbstractOrganizationUser> findByOrganizationIdOrderByDisplayNameAsc(Long id);
  List<AbstractOrganizationUser> findUnverifiedUserByOrganization(Long id);

  @Transactional(propagation = Propagation.REQUIRES_NEW)
  @Query("FROM AbstractOrganizationUser WHERE id = ?1")
  AbstractOrganizationUser findOneInNewTransaction(Long id);
}
