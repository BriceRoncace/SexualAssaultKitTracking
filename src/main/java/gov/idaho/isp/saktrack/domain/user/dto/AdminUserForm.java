package gov.idaho.isp.saktrack.domain.user.dto;

import gov.idaho.isp.saktrack.domain.user.AbstractUser;
import gov.idaho.isp.saktrack.domain.user.AdminUser;
import gov.idaho.isp.saktrack.validation.PasswordPresentIfRequired;
import javax.validation.Valid;

@PasswordPresentIfRequired
public class AdminUserForm extends UserForm {
  @Valid
  private AdminUser adminUser;

  public AdminUser getAdminUser() {
    return adminUser;
  }

  public void setAdminUser(AdminUser adminUser) {
    this.adminUser = adminUser;
  }

  @Override
  public AbstractUser getAbstractUser() {
    return adminUser;
  }
}
