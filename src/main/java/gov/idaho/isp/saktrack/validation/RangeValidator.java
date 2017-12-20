package gov.idaho.isp.saktrack.validation;

import gov.idaho.isp.saktrack.util.RangeParserUtil;
import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;

public class RangeValidator implements ConstraintValidator<Range, String> {

  @Override
  public void initialize(Range constraintAnnotation) {
  }

  @Override
  public boolean isValid(String value, ConstraintValidatorContext context) {
    if (value == null) {
      return true;
    }
    try {
      RangeParserUtil.parse(value);
      return true;
    }
    catch (IllegalArgumentException ex) {
      return false;
    }
  }
}
