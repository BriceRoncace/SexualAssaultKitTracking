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

import gov.idaho.isp.saktrack.util.reflection.PropertyUtils;
import java.util.Objects;
import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;

public class PropertiesEqualValidator implements ConstraintValidator<PropertiesEqual, Object> {
  private String propertyNameOne;
  private String propertyNameTwo;

  @Override
  public void initialize(PropertiesEqual a) {
    propertyNameOne = a.propertyNameOne();
    propertyNameTwo = a.propertyNameTwo();
  }

  @Override
  public boolean isValid(Object obj, ConstraintValidatorContext cvc) {
    Object one = PropertyUtils.getPropertyOrNull(obj, propertyNameOne);
    Object two = PropertyUtils.getPropertyOrNull(obj, propertyNameTwo);
    return Objects.equals(two, one);
  }
}
