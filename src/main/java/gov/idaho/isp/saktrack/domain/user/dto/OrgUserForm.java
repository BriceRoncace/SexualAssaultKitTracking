package gov.idaho.isp.saktrack.domain.user.dto;

import gov.idaho.isp.saktrack.domain.user.AbstractUser;
import gov.idaho.isp.saktrack.domain.user.organization.AbstractOrganizationUser;
import gov.idaho.isp.saktrack.validation.PasswordPresentIfRequired;
import javax.validation.Valid;

@PasswordPresentIfRequired
public class OrgUserForm extends UserForm {
  @Valid
  private AbstractOrganizationUser orgUser;

  public AbstractOrganizationUser getOrgUser() {
    return orgUser;
  }

  public void setOrgUser(AbstractOrganizationUser orgUser) {
    this.orgUser = orgUser;
  }

  @Override
  public AbstractUser getAbstractUser() {
    return orgUser;
  }
}