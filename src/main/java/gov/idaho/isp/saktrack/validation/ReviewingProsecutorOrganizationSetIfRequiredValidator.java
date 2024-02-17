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

import gov.idaho.isp.saktrack.domain.SexualAssaultKit;
import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;

public class ReviewingProsecutorOrganizationSetIfRequiredValidator implements ConstraintValidator<ReviewingProsecutorOrganizationSetIfRequired, SexualAssaultKit> {
  @Override
  public void initialize(ReviewingProsecutorOrganizationSetIfRequired constraintAnnotation) {
  }

  @Override
  public boolean isValid(SexualAssaultKit kit, ConstraintValidatorContext context) {
    if (kit == null || kit.getMedicalDetails() == null || kit.getMedicalDetails().getRequestingLeAgency() == null || kit.getLeDetails() == null) {
      return true;
    }

    if (kit.getMedicalDetails().getRequestingLeAgency().isStatewide() && Boolean.FALSE.equals(kit.getLeDetails().getMeetsSubmissionCriteria())) {
      return kit.getLegalDetails().getReviewingOrganization() != null;
    }

    return true;
  }

}
