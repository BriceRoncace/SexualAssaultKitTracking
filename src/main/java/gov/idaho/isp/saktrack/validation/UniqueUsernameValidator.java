package gov.idaho.isp.saktrack.validation;

import gov.idaho.isp.ldap.user.LdapUserDirectory;
import gov.idaho.isp.saktrack.user.User;
import gov.idaho.isp.saktrack.user.User.AuthMethod;
import gov.idaho.isp.saktrack.user.persistence.AdminUserRepository;
import gov.idaho.isp.saktrack.user.persistence.OrganizationUserRepository;
import gov.idaho.isp.saktrack.util.reflection.PropertyUtils;
import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;

public class UniqueUsernameValidator implements ConstraintValidator<UniqueUsername, Object> {
  private OrganizationUserRepository organizationUserRepository;
  private AdminUserRepository adminUserRepository;
  private LdapUserDirectory ldapUserDirectory;
  private String userProperty;
  private Boolean ldapAuthEnabled;

  @Override
  public void initialize(UniqueUsername a) {
    userProperty = a.userProperty();
  }

  @Override
  public boolean isValid(Object object, ConstraintValidatorContext cvc) {
    User user = getUser(object);
    if (organizationUserRepository != null && adminUserRepository != null && ldapUserDirectory != null) {
      if (isAlreadyAnOrgUser(user) || isAlreadyAnAdminUser(user) || (user.getAuthMethod() != AuthMethod.LDAP && isAlreadyAnLdapUser(user))) {
        return false;
      }
    }
    return true;
  }

  private User getUser(Object object) {
    if (StringUtils.isNotBlank(userProperty)) {
      return PropertyUtils.getProperty(object, userProperty, User.class);
    }
    else if (object instanceof User) {
      return (User) object;
    }
    throw new IllegalStateException("UniqueUsername validation constraint misconfigured: Anotation must be applied to User type or specify user property as an annotation attribute.");
  }

  private boolean isAlreadyAnOrgUser(User user) {
    return differentEntities(organizationUserRepository.findByUsernameIgnoreCase(user.getUsername()), user);
  }

  private boolean isAlreadyAnAdminUser(User user) {
    return differentEntities(adminUserRepository.findByUsernameIgnoreCase(user.getUsername()), user);
  }

  private boolean isAlreadyAnLdapUser(User user) {
    if (Boolean.TRUE.equals(ldapAuthEnabled)) {
      return ldapUserDirectory.loadByUsername(user.getUsername()) != null;
    }
    return false;
  }

  private boolean differentEntities(User existingUser, User user) {
    return existingUser != null && !existingUser.getId().equals(user.getId());
  }

  @Autowired
  public void setOrganizationUserRepository(OrganizationUserRepository organizationUserRepository) {
    this.organizationUserRepository = organizationUserRepository;
  }

  @Autowired
  public void setAdminUserRepository(AdminUserRepository adminUserRepository) {
    this.adminUserRepository = adminUserRepository;
  }

  @Autowired
  public void setLdapUserDirectory(LdapUserDirectory ldapUserDirectory) {
    this.ldapUserDirectory = ldapUserDirectory;
  }

  @Value("${ldap.auth.enabled}")
  public void setLdapAuthEnabled(Boolean ldapAuthEnabled) {
    this.ldapAuthEnabled = ldapAuthEnabled;
  }
}
