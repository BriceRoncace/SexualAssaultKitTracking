package gov.idaho.isp.saktrack.security;

import gov.idaho.isp.saktrack.user.AdminUser;
import gov.idaho.isp.saktrack.user.User;
import gov.idaho.isp.saktrack.user.User.AuthMethod;
import gov.idaho.isp.saktrack.user.organization.OrganizationUser;
import gov.idaho.isp.saktrack.user.persistence.AdminUserRepository;
import gov.idaho.isp.saktrack.user.persistence.OrganizationUserRepository;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.ldap.core.DirContextOperations;
import org.springframework.security.authentication.DisabledException;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.security.ldap.userdetails.LdapUserDetailsMapper;
import org.springframework.stereotype.Component;

@Component
public class CustomLdapUserDetailsMapper extends LdapUserDetailsMapper {
  private AdminUserRepository adminUserRepository;
  private OrganizationUserRepository organizationUserRepository;

  @Override
  public UserDetails mapUserFromContext(DirContextOperations ctx, String username, Collection<? extends GrantedAuthority> ldapAuthorities) {
    AdminUser adminUser = adminUserRepository.findByUsernameIgnoreCase(username);
    if (adminUser != null && User.Type.ADMIN.equals(adminUser.getType())) {
      UserDetails userDetails = super.mapUserFromContext(ctx, username, Arrays.asList(new SimpleGrantedAuthority(User.Type.ADMIN.toString())));
      adminUser.setUserDetails(userDetails);
      return adminUser;
    }

    OrganizationUser orgUser = organizationUserRepository.findByUsernameIgnoreCase(username);
    if (orgUser != null && orgUser.getAuthMethod() == AuthMethod.LDAP) {
      if (!orgUser.isActive()) {
        throw new DisabledException("User is disabled");
      }
      UserDetails userDetails = super.mapUserFromContext(ctx, username, getAuthorities(orgUser));
      orgUser.setUserDetails(userDetails);
      return orgUser;
    }

    throw new UsernameNotFoundException("User not found with username [" + username + "]");
  }

  private List<GrantedAuthority> getAuthorities(OrganizationUser orgUser) {
    List<GrantedAuthority> auths = new ArrayList<>();
    auths.add(new SimpleGrantedAuthority(orgUser.getType().toString()));
    if (Boolean.TRUE.equals(orgUser.isOrganizationAdmin())) {
      auths.add(new SimpleGrantedAuthority("ORG_ADMIN"));
    }
    return auths;
  }

  @Autowired
  public void setAdminUserRepository(AdminUserRepository adminUserRepository) {
    this.adminUserRepository = adminUserRepository;
  }

  @Autowired
  public void setOrganizationUserRepository(OrganizationUserRepository organizationUserRepository) {
    this.organizationUserRepository = organizationUserRepository;
  }
}
