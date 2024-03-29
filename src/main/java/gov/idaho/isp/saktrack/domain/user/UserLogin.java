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

import gov.idaho.isp.saktrack.domain.user.organization.OrganizationUser;
import java.io.Serializable;
import java.time.LocalDateTime;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.Id;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;

@Entity
public class UserLogin implements Serializable {
  @Id
  @GeneratedValue
  private Long id;

  private Long userId;

  @NotBlank(message = "{username.blank}")
  private String username;

  private String displayName;

  private String email;

  private String organization;

  private Long organizationId;

  @NotNull(message = "{user.type.null}")
  @Enumerated(EnumType.STRING)
  private User.Type userType;

  private boolean organizationAdmin;

  @NotNull(message = "{login.time.null}")
  private LocalDateTime loginTime;

  private String userAgent;

  protected UserLogin() {}

  public UserLogin(User user, String userAgent) {
    this.userId = user.getId();
    this.userType = user.getType();
    this.organizationAdmin = user.isOrganizationAdmin();
    this.username = user.getUsername();
    this.displayName = user.getDisplayName();
    this.email = user.getEmail();
    this.loginTime = LocalDateTime.now();
    this.userAgent = userAgent;
    if (user instanceof OrganizationUser) {
      OrganizationUser orgUser = (OrganizationUser) user;
      this.organizationId = orgUser.getOrganization().getId();
      this.organization = orgUser.getOrganization().getName();
    }
  }

  public Long getId() {
    return id;
  }

  public void setId(Long id) {
    this.id = id;
  }

  public Long getUserId() {
    return userId;
  }

  public void setUserId(Long userId) {
    this.userId = userId;
  }

  public String getUsername() {
    return username;
  }

  public void setUsername(String username) {
    this.username = username;
  }

  public String getDisplayName() {
    return displayName;
  }

  public void setDisplayName(String displayName) {
    this.displayName = displayName;
  }

  public String getEmail() {
    return email;
  }

  public void setEmail(String email) {
    this.email = email;
  }

  public String getOrganization() {
    return organization;
  }

  public void setOrganization(String organization) {
    this.organization = organization;
  }

  public Long getOrganizationId() {
    return organizationId;
  }

  public void setOrganizationId(Long organizationId) {
    this.organizationId = organizationId;
  }

  public User.Type getUserType() {
    return userType;
  }

  public void setUserType(User.Type userType) {
    this.userType = userType;
  }

  public boolean isOrganizationAdmin() {
    return organizationAdmin;
  }

  public void setOrganizationAdmin(boolean organizationAdmin) {
    this.organizationAdmin = organizationAdmin;
  }

  public LocalDateTime getLoginTime() {
    return loginTime;
  }

  public void setLoginTime(LocalDateTime loginTime) {
    this.loginTime = loginTime;
  }

  public String getUserAgent() {
    return userAgent;
  }

  public void setUserAgent(String userAgent) {
    this.userAgent = userAgent;
  }

  @Override
  public String toString() {
    return "UserLogin{" + "id=" + id + ", userId=" + userId + ", username=" + username + ", displayName=" + displayName + ", email=" + email + ", organization=" + organization + ", organizationId=" + organizationId + ", userType=" + userType + ", organizationAdmin=" + organizationAdmin + ", loginTime=" + loginTime + ", userAgent=" + userAgent + '}';
  }
}
