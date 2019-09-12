/* 
 * Copyright 2017 Idaho State Police.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package gov.idaho.isp.saktrack.security;

import gov.idaho.isp.saktrack.domain.user.AbstractUser;
import gov.idaho.isp.saktrack.domain.user.AbstractUserRepository;
import gov.idaho.isp.saktrack.domain.user.AdminUser;
import gov.idaho.isp.saktrack.domain.user.organization.OrganizationUser;
import java.util.ArrayList;
import java.util.List;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.stereotype.Component;

@Component
public class CustomDatabaseUserDetailsService implements UserDetailsService {
  private final AbstractUserRepository abstractUserRepository;

  public CustomDatabaseUserDetailsService(AbstractUserRepository abstractUserRepository) {
    this.abstractUserRepository = abstractUserRepository;
  }

  @Override
  public UserDetails loadUserByUsername(String username) throws UsernameNotFoundException {
    AbstractUser user = abstractUserRepository.findByUsernameIgnoreCase(username);
    if (user == null) {
      throw new UsernameNotFoundException("User not found with username [" + username + "]");
    }

    return user.isAdmin() ? loadAdminUser((AdminUser) user) : loadOrganizationUser((OrganizationUser) user);
  }

  private UserDetails loadAdminUser(AdminUser user) {
    user.setUserDetails(new org.springframework.security.core.userdetails.User(user.getUsername(), user.getPassword(), user.isEnabled(), true, true, true, getAuthorities(user)));
    return user;
  }

  private List<GrantedAuthority> getAuthorities(AdminUser user) {
    List<GrantedAuthority> auths = new ArrayList<>();
    auths.add(new SimpleGrantedAuthority("ADMIN"));
    return auths;
  }

  private UserDetails loadOrganizationUser(OrganizationUser user) {
    user.setUserDetails(new org.springframework.security.core.userdetails.User(user.getUsername(), user.getPassword(), user.isActive(), true, true, user.isVerified(), getAuthorities(user)));
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
}