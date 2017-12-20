package gov.idaho.isp.saktrack.user.password.dto;

import gov.idaho.isp.saktrack.validation.OldPasswordPresentAndValid;

@OldPasswordPresentAndValid
public class ChangePasswordRequest extends SimplePasswordPair {
  private Long userId;
  private String oldPassword;

  public Long getUserId() {
    return userId;
  }

  public void setUserId(Long userId) {
    this.userId = userId;
  }

  public String getOldPassword() {
    return oldPassword;
  }

  public void setOldPassword(String oldPassword) {
    this.oldPassword = oldPassword;
  }

  @Override
  public String toString() {
    return "ChangePasswordRequest{" + "userId=" + userId + ", oldPassword=" + oldPassword + ", super=" + super.toString() + '}';
  }
}