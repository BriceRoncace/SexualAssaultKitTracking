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

import gov.idaho.isp.saktrack.service.SerialNumberFormatter;
import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import org.springframework.beans.factory.annotation.Autowired;

public class SerialNumberValidator implements ConstraintValidator<SerialNumberValid, Object> {
  @Autowired
  private SerialNumberFormatter serialNumberFormatter;

  @Override
  public boolean isValid(Object object, ConstraintValidatorContext cvc) {
    if (serialNumberFormatter == null || object == null) {
      return true;
    }

    String val = object.toString();
    return serialNumberFormatter.isValid(val) && serialNumberFormatter.format(val).equals(val);
  }
}
