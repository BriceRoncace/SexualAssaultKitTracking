package gov.idaho.isp.saktrack.validation;

import gov.idaho.isp.saktrack.domain.user.AbstractUser;
import gov.idaho.isp.saktrack.domain.user.dto.UserForm;
import gov.idaho.isp.saktrack.domain.user.password.dto.PasswordPair;
import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;
import org.apache.commons.lang3.StringUtils;

public class PasswordPresentIfRequiredValidator implements ConstraintValidator<PasswordPresentIfRequired, UserForm> {

  @Override
  public void initialize(PasswordPresentIfRequired a) {
  }

  @Override
  public boolean isValid(UserForm userForm, ConstraintValidatorContext cvc) {
    if (userForm == null || userForm.getAbstractUser() == null || userForm.getPasswordPair() == null) {
      return true;
    }

    AbstractUser user = userForm.getAbstractUser();
    PasswordPair passwordPair = userForm.getPasswordPair();
    return !(user.getId() == null && StringUtils.isBlank(passwordPair.getPasswordOne()));
  }
}
