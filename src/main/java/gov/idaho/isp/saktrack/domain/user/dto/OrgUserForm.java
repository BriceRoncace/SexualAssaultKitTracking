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

package gov.idaho.isp.saktrack.domain.user.dto;

import gov.idaho.isp.saktrack.domain.user.AbstractUser;
import gov.idaho.isp.saktrack.domain.user.organization.AbstractOrganizationUser;
import gov.idaho.isp.saktrack.validation.PasswordPresentIfRequired;
import jakarta.validation.Valid;

@PasswordPresentIfRequired
public class OrgUserForm extends UserForm {
  @Valid
  private AbstractOrganizationUser orgUser;

  public AbstractOrganizationUser getOrgUser() {
    return orgUser;
  }

  public void setOrgUser(AbstractOrganizationUser orgUser) {
    this.orgUser = orgUser;
  }

  @Override
  public AbstractUser getAbstractUser() {
    return orgUser;
  }
}