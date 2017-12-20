package gov.idaho.isp.saktrack.user.view;

import gov.idaho.isp.saktrack.user.organization.AbstractOrganizationUser;
import gov.idaho.isp.saktrack.user.password.dto.PasswordPair;
import gov.idaho.isp.saktrack.validation.PasswordPresentIfRequired;
import javax.validation.Valid;
import org.apache.commons.lang3.StringUtils;
import org.springframework.security.crypto.password.PasswordEncoder;

@PasswordPresentIfRequired
public class DbUserForm {
  @Valid
  private AbstractOrganizationUser orgUser;

  @Valid
  private PasswordPair passwordPair;

  public AbstractOrganizationUser getOrgUser() {
    return orgUser;
  }

  public void setOrgUser(AbstractOrganizationUser orgUser) {
    this.orgUser = orgUser;
  }

  public PasswordPair getPasswordPair() {
    return passwordPair;
  }

  public void setPasswordPair(PasswordPair passwordPair) {
    this.passwordPair = passwordPair;
  }

  public void encodePasswordIfNecessary(PasswordEncoder passwordEncoder) {
    if (passwordPair != null && StringUtils.isNotBlank(passwordPair.getPasswordOne())) {
      orgUser.setPassword(passwordEncoder.encode(passwordPair.getPasswordOne()));
    }
  }
}
