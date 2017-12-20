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
