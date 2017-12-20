package gov.idaho.isp.saktrack.security;

import gov.idaho.isp.saktrack.user.User.AuthMethod;
import gov.idaho.isp.saktrack.user.organization.OrganizationUser;
import gov.idaho.isp.saktrack.user.persistence.OrganizationUserRepository;
import java.util.ArrayList;
import java.util.List;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.stereotype.Component;

@Component
public class CustomDatabaseUserDetailsService implements UserDetailsService {
  private OrganizationUserRepository organizationUserRepository;

  @Override
  public UserDetails loadUserByUsername(String username) throws UsernameNotFoundException {
    OrganizationUser user = organizationUserRepository.findByUsernameIgnoreCase(username);
    if (user == null || user.getAuthMethod() != AuthMethod.DATABASE) {
      throw new UsernameNotFoundException("User not found with username [" + username + "]");
    }
    user.setUserDetails(new org.springframework.security.core.userdetails.User(username, user.getPassword(), user.isActive(), true, true, user.isVerified(), getAuthorities(user)));
    return user;
  }

  private List<GrantedAuthority> getAuthorities(OrganizationUser user) {
    List<GrantedAuthority> auths = new ArrayList<>();
    auths.add(new SimpleGrantedAuthority(user.getType().toString()));
    if (Boolean.TRUE.equals(user.isOrganizationAdmin())) {
      auths.add(new SimpleGrantedAuthority("ORG_ADMIN"));
    }
    return auths;
  }

  @Autowired
  public void setOrganizationUserRepository(OrganizationUserRepository organizationUserRepository) {
    this.organizationUserRepository = organizationUserRepository;
  }
}