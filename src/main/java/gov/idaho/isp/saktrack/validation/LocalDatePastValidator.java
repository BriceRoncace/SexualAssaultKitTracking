package gov.idaho.isp.saktrack.validation;

import java.time.LocalDate;
import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;

public class LocalDatePastValidator implements ConstraintValidator<LocalDatePast, LocalDate> {
  @Override
  public void initialize(LocalDatePast a) {
  }

  @Override
  public boolean isValid(LocalDate localDate, ConstraintValidatorContext cvc) {
    if (localDate == null) {
      return true;
    }
    return !localDate.isAfter(LocalDate.now());
  }
}
