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

package gov.idaho.isp.saktrack.util.beanvalidation;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import java.util.stream.Collectors;
import javax.validation.ConstraintViolation;
import javax.validation.ConstraintViolationException;

public class BeanValidationUtils {

  public static Set<ConstraintViolation<?>> getConstraintViolations(Throwable ex) {
    Set<ConstraintViolation<?>> violations = new HashSet<>();
    Throwable cause = ex.getCause();
    while (cause != null) {
      if (cause instanceof javax.validation.ConstraintViolationException) {
        ConstraintViolationException constraintViolationEx = (javax.validation.ConstraintViolationException) cause;
        violations.addAll(constraintViolationEx.getConstraintViolations());
      }
      cause = cause.getCause();
    }
    return violations;
  }

  public static <T> Set<String> getErrorMessages(Set<ConstraintViolation<T>> violations) {
    if (violations != null && !violations.isEmpty()) {
      return violations.stream().map((ConstraintViolation cv) -> cv.getMessage()).collect(Collectors.toSet());
    }
    return Collections.emptySet();
  }

  public static Set<String> getConstraintViolationErrorMessages(Throwable ex) {
    Set<ConstraintViolation<?>> violations = getConstraintViolations(ex);
    return violations.stream().map((ConstraintViolation cv) -> cv.getMessage()).collect(Collectors.toSet());
  }
}