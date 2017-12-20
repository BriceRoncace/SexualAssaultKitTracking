package gov.idaho.isp.saktrack.service;

import gov.idaho.isp.ldap.user.LdapUser;
import gov.idaho.isp.ldap.user.LdapUserDirectory;
import gov.idaho.isp.saktrack.organization.Organization;
import gov.idaho.isp.saktrack.organization.OrganizationRepository;
import gov.idaho.isp.saktrack.user.User;
import gov.idaho.isp.saktrack.user.User.AuthMethod;
import gov.idaho.isp.saktrack.user.organization.AbstractOrganizationUser;
import gov.idaho.isp.saktrack.user.password.dto.ChangePasswordRequest;
import gov.idaho.isp.saktrack.user.password.dto.PasswordPair;
import gov.idaho.isp.saktrack.user.password.dto.SimplePasswordPair;
import gov.idaho.isp.saktrack.user.persistence.OrganizationUserRepository;
import gov.idaho.isp.saktrack.user.view.DbUserForm;
import gov.idaho.isp.saktrack.user.view.LdapUserForm;
import java.time.LocalDate;
import java.util.Optional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class OrganizationUserPreparerImpl implements OrganizationUserPreparer {
  private OrganizationRepository organizationRepository;
  private OrganizationUserRepository organizationUserRepository;
  private LdapUserDirectory ldapUserDirectory;

  @Override
  public DbUserForm prepareVerifiedDbUserForm(Optional<Long> userId, Long organizationId, boolean hasCurrentPassword) {
    DbUserForm dbUserForm = new DbUserForm();
    dbUserForm.setOrgUser(loadOrCreateVerifiedUser(userId, organizationId, AuthMethod.DATABASE));
    dbUserForm.setPasswordPair(preparePasswordPair(userId, hasCurrentPassword));
    return dbUserForm;
  }

  @Override
  public DbUserForm prepareNewDbUserForm(Optional<Long> organizationId) {
    DbUserForm dbUserForm = new DbUserForm();
    dbUserForm.setPasswordPair(new SimplePasswordPair());
    dbUserForm.setOrgUser(createUnverifiedUser(organizationId));
    return dbUserForm;
  }

  @Override
  public LdapUserForm prepareVerifiedLdapUserForm(Optional<Long> userId, Long organizationId, String ldapUsername) {
    LdapUserForm ldapUserForm = new LdapUserForm();
    ldapUserForm.setLdapUsername(ldapUsername);
    AbstractOrganizationUser orgUser = loadOrCreateVerifiedUser(userId, organizationId, AuthMethod.LDAP);
    copyLdapAttributes(orgUser, ldapUserDirectory.loadByUsername(ldapUsername));
    ldapUserForm.setOrgUser(orgUser);
    return ldapUserForm;
  }

  private void copyLdapAttributes(AbstractOrganizationUser user, LdapUser ldapUser) {
    if (ldapUser != null) {
      user.setUsername(ldapUser.getUsername());
      user.setDisplayName(getDisplayName(ldapUser));
      user.setEmail(ldapUser.getEmail());
      user.setPhone(ldapUser.getPhone());
    }
  }

  private AbstractOrganizationUser loadOrCreateVerifiedUser(Optional<Long> userId, Long organizationId, AuthMethod authMethod) {
    Organization organization = organizationRepository.findOne(organizationId);
    AbstractOrganizationUser orgUser = userId.isPresent() ? loadOrganizationUser(userId.get()) : getNewUser(organization);
    orgUser.setOrganization(organization);
    orgUser.setPasskey(organization.getPasskey());
    orgUser.setAuthMethod(authMethod);
    if (orgUser.getVerifiedDate() == null) {
      orgUser.setVerifiedDate(LocalDate.now());
    }
    return orgUser;
  }

  private AbstractOrganizationUser createUnverifiedUser(Optional<Long> organizationId) {
    if (!organizationId.isPresent()) {
      return nullOrganizationUser();
    }
    Organization organization = organizationRepository.findOne(organizationId.get());
    AbstractOrganizationUser orgUser = getNewUser(organization);
    orgUser.setAuthMethod(User.AuthMethod.DATABASE);
    orgUser.setOrganization(organization);
    return orgUser;
  }

  private AbstractOrganizationUser nullOrganizationUser() {
    return new AbstractOrganizationUser() {
      @Override
      public User.Type getType() {
        return null;
      }
    };
  }

  private PasswordPair preparePasswordPair(Optional<Long> userId, boolean hasCurrentPassword) {
    if (hasCurrentPassword) {
      ChangePasswordRequest req = new ChangePasswordRequest();
      if (userId.isPresent()) {
        req.setUserId(userId.get());
      }
      return req;
    }

    return new SimplePasswordPair();
  }

  private String getDisplayName(LdapUser ldapUser) {
    return ldapUser.getFirstName() + " " + ldapUser.getLastName();
  }

  private AbstractOrganizationUser loadOrganizationUser(Long userId) {
    return organizationUserRepository.findOne(userId);
  }

  private AbstractOrganizationUser getNewUser(Organization organization) {
    return organization.getType().getNewUser();
  }

  @Autowired
  public void setOrganizationRepository(OrganizationRepository organizationRepository) {
    this.organizationRepository = organizationRepository;
  }

  @Autowired
  public void setOrganizationUserRepository(OrganizationUserRepository organizationUserRepository) {
    this.organizationUserRepository = organizationUserRepository;
  }

  @Autowired
  public void setLdapUserDirectory(LdapUserDirectory ldapUserDirectory) {
    this.ldapUserDirectory = ldapUserDirectory;
  }
}