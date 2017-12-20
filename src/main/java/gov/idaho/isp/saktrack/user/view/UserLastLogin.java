package gov.idaho.isp.saktrack.user.view;

import gov.idaho.isp.saktrack.organization.OrganizationType;
import java.time.LocalDateTime;

public class UserLastLogin {
  private Long userId;
  private String username;
  private String userDisplayName;
  private Long organizationId;
  private String organization;
  private OrganizationType organizationType;
  private LocalDateTime lastLogin;

  public UserLastLogin(Long userId, String username, String userDisplayName, Long orgId, String orgName, String orgType, LocalDateTime lastLogin) {
    this.userId = userId;
    this.username = username;
    this.userDisplayName = userDisplayName;
    if (orgId != null) {
      this.organizationId = orgId;
      this.organization = orgName;
      this.organizationType = OrganizationType.valueOf(orgType);
    }
    this.lastLogin = lastLogin;
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

  public String getUserDisplayName() {
    return userDisplayName;
  }

  public void setUserDisplayName(String userDisplayName) {
    this.userDisplayName = userDisplayName;
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

  public OrganizationType getOrganizationType() {
    return organizationType;
  }

  public void setOrganizationType(OrganizationType organizationType) {
    this.organizationType = organizationType;
  }

  public LocalDateTime getLastLogin() {
    return lastLogin;
  }

  public void setLastLogin(LocalDateTime lastLogin) {
    this.lastLogin = lastLogin;
  }

  @Override
  public String toString() {
    return "UserLastLogin{" + "userId=" + userId + ", userDisplayName=" + userDisplayName + ", organizationId=" + organizationId + ", organization=" + organization + ", organizationType=" + organizationType + ", lastLogin=" + lastLogin + '}';
  }
}