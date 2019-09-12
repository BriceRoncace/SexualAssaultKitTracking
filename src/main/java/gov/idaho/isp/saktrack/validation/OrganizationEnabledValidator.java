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

package gov.idaho.isp.saktrack.validation;

import gov.idaho.isp.saktrack.domain.organization.Organization;
import gov.idaho.isp.saktrack.domain.organization.OrganizationRepository;
import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;
import org.springframework.beans.factory.annotation.Autowired;

public class OrganizationEnabledValidator implements ConstraintValidator<OrganizationEnabled, Long> {
  private OrganizationRepository organizationRepository;

  @Override
  public void initialize(OrganizationEnabled a) {
  }

  @Override
  public boolean isValid(Long orgId, ConstraintValidatorContext cvc) {
    if (organizationRepository != null) {
      Organization org = organizationRepository.findById(orgId).orElse(null);
      if (org != null) {
        return org.getEnabled();
      }
      return false;
    }
    return true;
  }

  @Autowired
  public void setOrganizationRepository(OrganizationRepository organizationRepository) {
    this.organizationRepository = organizationRepository;
  }
}
