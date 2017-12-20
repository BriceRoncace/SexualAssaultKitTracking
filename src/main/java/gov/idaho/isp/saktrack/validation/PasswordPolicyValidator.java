package gov.idaho.isp.saktrack.validation;

import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;
import org.apache.commons.lang3.StringUtils;

public class PasswordPolicyValidator implements ConstraintValidator<PasswordPolicy, String> {
  private int capitals;
  private int numbers;
  private int specials;
  private int minSize;
  private int maxSize;

  @Override
  public void initialize(PasswordPolicy a) {
    capitals = a.capitals();
    numbers = a.numbers();
    specials = a.specials();
    minSize = a.minSize();
    maxSize = a.maxSize();
  }

  @Override
  public boolean isValid(String str, ConstraintValidatorContext cvc) {
    if (StringUtils.isBlank(str)) {
      return true;
    }
    return isCapitalsValid(str) && isNumbersValid(str) && isSpecialsValid(str) && isAtLeastMinSize(str) && isNoMoreThanMaxSize(str);
  }

  private boolean isCapitalsValid(String str) {
    return str.replaceAll("[^A-Z]", "").length() >= capitals;
  }

  private boolean isNumbersValid(String str) {
    return str.replaceAll("[^0-9]", "").length() >= numbers;
  }

  private boolean isSpecialsValid(String str) {
    return str.replaceAll("[a-zA-Z0-9]", "").length() >= specials;
  }

  private boolean isAtLeastMinSize(String str) {
    return str.length() >= minSize;
  }

  private boolean isNoMoreThanMaxSize(String str) {
    return str.length() <= maxSize;
  }
}