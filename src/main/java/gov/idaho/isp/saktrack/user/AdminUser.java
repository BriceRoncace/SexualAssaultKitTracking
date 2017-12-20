package gov.idaho.isp.saktrack.user;

import gov.idaho.isp.ldap.user.LdapUser;
import java.io.Serializable;
import javax.persistence.DiscriminatorValue;
import javax.persistence.Entity;
import javax.persistence.Inheritance;
import javax.persistence.InheritanceType;

@Entity
@DiscriminatorValue(value = "Admin")
@Inheritance(strategy = InheritanceType.SINGLE_TABLE)
public class AdminUser extends AbstractUser implements User, Serializable {

  public AdminUser() {
    this(AuthMethod.LDAP);
  }

  public AdminUser(AuthMethod auhtMethod) {
    this.authMethod = auhtMethod;
  }

  public AdminUser(LdapUser ldapUser) {
    this.authMethod = AuthMethod.LDAP;
    initLdapAttributes(ldapUser);
  }

  private void initLdapAttributes(LdapUser ldapUser) {
    setUsername(ldapUser.getUsername());
    setDisplayName(ldapUser.getFirstName() + " " + ldapUser.getLastName());
    setPhone(ldapUser.getPhone());
    setEmail(ldapUser.getEmail());
  }

  @Override
  public User.Type getType() {
    return User.Type.ADMIN;
  }

  @Override
  public boolean isOrganizationAdmin() {
    return true;
  }
}