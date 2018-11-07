package gov.idaho.isp.saktrack.validation;

import gov.idaho.isp.saktrack.domain.user.organization.OrganizationUser;
import gov.idaho.isp.saktrack.domain.user.password.dto.ChangePasswordRequest;
import gov.idaho.isp.saktrack.domain.user.organization.OrganizationUserRepository;
import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.crypto.password.PasswordEncoder;

public class OldPasswordPresentAndValidValidator implements ConstraintValidator<OldPasswordPresentAndValid, ChangePasswordRequest> {
  private OrganizationUserRepository organizationUserRepository;
  private PasswordEncoder passwordEncoder;

  @Override
  public void initialize(OldPasswordPresentAndValid constraintAnnotation) {
  }

  @Override
  public boolean isValid(ChangePasswordRequest req, ConstraintValidatorContext context) {
    if (req == null || passwordEncoder == null || organizationUserRepository == null) {
      return true;
    }

    if (!allBlankOrAllPresent(req.getOldPassword(), req.getPasswordOne(), req.getPasswordTwo())) {
      return false;
    }

    if (req.getOldPassword() != null && !passwordEncoder.matches(req.getOldPassword(), getCurrentPassword(req))) {
      return false;
    }

    return true;
  }

  private String getCurrentPassword(ChangePasswordRequest req) {
    OrganizationUser user = organizationUserRepository.findOneInNewTransaction(req.getUserId());
    return user.getPassword();
  }

  private boolean allBlankOrAllPresent(String... values) {
    return allBlank(values) || allPresent(values);
  }

  private boolean allBlank(String... values) {
    for (String val : values) {
      if (StringUtils.isNotBlank(val)) {
        return false;
      }
    }
    return true;
  }

  private boolean allPresent(String... values) {
    return StringUtils.isNoneBlank(values);
  }

  @Autowired
  public void setOrganizationUserRepository(OrganizationUserRepository organizationUserRepository) {
    this.organizationUserRepository = organizationUserRepository;
  }

  @Autowired
  public void setPasswordEncoder(PasswordEncoder passwordEncoder) {
    this.passwordEncoder = passwordEncoder;
  }
}