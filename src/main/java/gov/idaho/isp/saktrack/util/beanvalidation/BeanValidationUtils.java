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