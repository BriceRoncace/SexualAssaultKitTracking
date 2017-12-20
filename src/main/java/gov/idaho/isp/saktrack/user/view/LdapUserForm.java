package gov.idaho.isp.saktrack.user.view;

import gov.idaho.isp.saktrack.user.organization.AbstractOrganizationUser;
import gov.idaho.isp.saktrack.validation.UniqueUsername;
import org.hibernate.validator.constraints.NotBlank;

@UniqueUsername(userProperty = "orgUser")
public class LdapUserForm {
  @NotBlank(message = "{username.blank}")
  private String ldapUsername;
  private AbstractOrganizationUser orgUser;

  public String getLdapUsername() {
    return ldapUsername;
  }

  public void setLdapUsername(String ldapUsername) {
    this.ldapUsername = ldapUsername;
  }

  public AbstractOrganizationUser getOrgUser() {
    return orgUser;
  }

  public void setOrgUser(AbstractOrganizationUser orgUser) {
    this.orgUser = orgUser;
  }
}
