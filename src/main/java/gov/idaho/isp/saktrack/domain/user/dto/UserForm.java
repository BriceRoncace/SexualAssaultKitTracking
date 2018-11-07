package gov.idaho.isp.saktrack.domain.user.dto;

import gov.idaho.isp.saktrack.domain.user.AbstractUser;
import gov.idaho.isp.saktrack.domain.user.password.dto.PasswordPair;
import javax.validation.Valid;
import org.apache.commons.lang3.StringUtils;
import org.springframework.security.crypto.password.PasswordEncoder;

public abstract class UserForm {
  @Valid
  private PasswordPair passwordPair;

  public PasswordPair getPasswordPair() {
    return passwordPair;
  }

  public void setPasswordPair(PasswordPair passwordPair) {
    this.passwordPair = passwordPair;
  }
  
  public abstract AbstractUser getAbstractUser();

  public void encodePasswordIfNecessary(PasswordEncoder passwordEncoder) {
    if (passwordPair != null && StringUtils.isNotBlank(passwordPair.getPasswordOne())) {
      getAbstractUser().setPassword(passwordEncoder.encode(passwordPair.getPasswordOne()));
    }
  }
}
