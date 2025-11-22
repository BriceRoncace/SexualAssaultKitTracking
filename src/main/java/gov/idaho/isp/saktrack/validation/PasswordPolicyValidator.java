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

import gov.idaho.isp.saktrack.Application;
import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;

public class PasswordPolicyValidator implements ConstraintValidator<PasswordPolicy, String> {
  @Autowired
  private Application.PasswordProperties passwordProperties;

  @Override
  public void initialize(PasswordPolicy a) {
    if (passwordProperties == null || passwordProperties.isEmpty()) {
      passwordProperties = new Application.PasswordProperties();
      passwordProperties.setMinLength(a.minLength());
      passwordProperties.setMaxLength(a.maxLength());
      passwordProperties.setCapitals(a.capitals());
      passwordProperties.setDigits(a.digits());
      passwordProperties.setSpecials(a.specials());
    }
  }

  @Override
  public boolean isValid(String str, ConstraintValidatorContext cvc) {
    if (StringUtils.isBlank(str)) {
      return true;
    }

    boolean valid = isCapitalsValid(str) && isDigitsValid(str) && isSpecialsValid(str) && isAtLeastMinSize(str) && isNoMoreThanMaxSize(str);
    if (!valid) {
      cvc.disableDefaultConstraintViolation();
      cvc.buildConstraintViolationWithTemplate(passwordProperties.getPasswordPolicy()).addConstraintViolation();
    }

    return valid;
  }

  private boolean isCapitalsValid(String str) {
    return str.replaceAll("[^A-Z]", "").length() >= passwordProperties.getCapitals();
  }

  private boolean isDigitsValid(String str) {
    return str.replaceAll("[^0-9]", "").length() >= passwordProperties.getDigits();
  }

  private boolean isSpecialsValid(String str) {
    return str.replaceAll("[a-zA-Z0-9]", "").length() >= passwordProperties.getSpecials();
  }

  private boolean isAtLeastMinSize(String str) {
    return str.length() >= passwordProperties.getMinLength();
  }

  private boolean isNoMoreThanMaxSize(String str) {
    return str.length() <= passwordProperties.getMaxLength();
  }
}