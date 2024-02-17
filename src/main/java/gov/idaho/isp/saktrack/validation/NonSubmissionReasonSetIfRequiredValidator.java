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

import gov.idaho.isp.saktrack.domain.LawEnforcementDetails;
import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;

public class NonSubmissionReasonSetIfRequiredValidator implements ConstraintValidator<NonSubmissionReasonSetIfRequired, LawEnforcementDetails> {

  @Override
  public void initialize(NonSubmissionReasonSetIfRequired constraintAnnotation) {
  }

  @Override
  public boolean isValid(LawEnforcementDetails leDetails, ConstraintValidatorContext context) {
    if (leDetails == null) {
      return true;
    }

    if (Boolean.FALSE.equals(leDetails.getMeetsSubmissionCriteria())) {
      return leDetails.getNonSubmissionReason() != null;
    }

    return true;
  }
}
