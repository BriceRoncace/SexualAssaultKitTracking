package gov.idaho.isp.saktrack.validation;

import gov.idaho.isp.saktrack.user.organization.AbstractOrganizationUser;
import gov.idaho.isp.saktrack.user.password.dto.PasswordPair;
import gov.idaho.isp.saktrack.user.view.DbUserForm;
import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;
import org.apache.commons.lang3.StringUtils;

public class PasswordPresentIfRequiredValidator implements ConstraintValidator<PasswordPresentIfRequired, DbUserForm> {

  @Override
  public void initialize(PasswordPresentIfRequired a) {
  }

  @Override
  public boolean isValid(DbUserForm userForm, ConstraintValidatorContext cvc) {
    if (userForm == null || userForm.getOrgUser() == null || userForm.getPasswordPair() == null) {
      return true;
    }

    AbstractOrganizationUser orgUser = userForm.getOrgUser();
    PasswordPair passwordPair = userForm.getPasswordPair();
    return !(orgUser.getId() == null && StringUtils.isBlank(passwordPair.getPasswordOne()));
  }
}
