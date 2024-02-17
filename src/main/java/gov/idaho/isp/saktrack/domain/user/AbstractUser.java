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

package gov.idaho.isp.saktrack.domain.user;

import gov.idaho.isp.saktrack.validation.UniqueUsername;
import java.io.Serializable;
import java.util.Collection;
import jakarta.persistence.DiscriminatorColumn;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.Id;
import jakarta.persistence.Index;
import jakarta.persistence.Inheritance;
import jakarta.persistence.InheritanceType;
import jakarta.persistence.Table;
import jakarta.persistence.Transient;
import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotBlank;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.userdetails.UserDetails;

@Entity
@Table(name = "UserAccount", indexes = {@Index(name = "AK_UserAccount_username", columnList="username", unique = true)})
@DiscriminatorColumn(name = "type")
@Inheritance(strategy = InheritanceType.SINGLE_TABLE)
@UniqueUsername
public abstract class AbstractUser implements User, Serializable {

  @Id
  @GeneratedValue
  private Long id;

  @NotBlank(message = "{username.blank}")
  private String username;

  private String password;

  @NotBlank(message = "{displayName.blank}")
  private String displayName;

  @NotBlank(message = "{email.blank}")
  @Email(message = "{email.not.valid}")
  private String email;

  @NotBlank(message = "{phone.blank}")
  private String phone;

  private boolean enabled;

  // Spring security user detail attributes
  @Transient
  private Collection<? extends GrantedAuthority> authorities;
  @Transient
  private boolean accountNonExpired;
  @Transient
  private boolean accountNonLocked;
  @Transient
  private boolean credentialsNonExpired;

  @Override
  public Long getId() {
    return id;
  }

  public void setId(Long id) {
    this.id = id;
  }

  @Override
  public String getUsername() {
    return username;
  }

  public void setUsername(String username) {
    this.username = username;
  }

  @Override
  public String getPassword() {
    return password;
  }

  public void setPassword(String password) {
    this.password = password;
  }

  @Override
  public String getDisplayName() {
    return displayName;
  }

  public void setDisplayName(String displayName) {
    this.displayName = displayName;
  }

  @Override
  public String getEmail() {
    return email;
  }

  public void setEmail(String email) {
    this.email = email;
  }

  @Override
  public String getPhone() {
    return phone;
  }

  public void setPhone(String phone) {
    this.phone = phone;
  }

  @Override
  public boolean isAdmin() {
    return this.getType() == Type.ADMIN;
  }

  @Override
  public boolean isEnabled() {
    return enabled;
  }

  public void setEnabled(boolean enabled) {
    this.enabled = enabled;
  }

  @Override
  public void setUserDetails(UserDetails userDetails) {
    this.username = userDetails.getUsername();
    this.password = userDetails.getPassword();
    this.authorities = userDetails.getAuthorities();
    this.accountNonExpired = userDetails.isAccountNonExpired();
    this.accountNonLocked = userDetails.isAccountNonLocked();
    this.credentialsNonExpired = userDetails.isCredentialsNonExpired();
    this.enabled = userDetails.isEnabled();
  }

  @Override
  public Collection<? extends GrantedAuthority> getAuthorities() {
    return authorities;
  }

  @Override
  public boolean isAccountNonExpired() {
    return accountNonExpired;
  }

  @Override
  public boolean isAccountNonLocked() {
    return accountNonLocked;
  }

  @Override
  public boolean isCredentialsNonExpired() {
    return credentialsNonExpired;
  }

  @Override
  public String toString() {
    return "AbstractUser{" + "id=" + id + ", username=" + username + ", displayName=" + displayName + ", email=" + email + ", phone=" + phone + ", authorities=" + authorities + '}';
  }
}
