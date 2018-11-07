package gov.idaho.isp.saktrack.domain.user.password.dto;

import gov.idaho.isp.saktrack.validation.PasswordPolicy;
import gov.idaho.isp.saktrack.validation.PropertiesEqual;
import javax.validation.constraints.NotBlank;

@PropertiesEqual(propertyNameOne = "passwordOne", propertyNameTwo = "passwordTwo", message = "{passwords.do.not.match}")
public class ResetPasswordPair implements PasswordPair {
  @NotBlank(message = "{request.blank}")
  private String request;

  @PasswordPolicy(minSize = 8, capitals = 1, numbers = 1, specials = 1)
  @NotBlank(message = "{reset.password.blank}")
  private String passwordOne;
  private String passwordTwo;

  public String getRequest() {
    return request;
  }

  public void setRequest(String request) {
    this.request = request;
  }

  @Override
  public String getPasswordOne() {
    return passwordOne;
  }

  @Override
  public void setPasswordOne(String passwordOne) {
    this.passwordOne = passwordOne;
  }

  @Override
  public String getPasswordTwo() {
    return passwordTwo;
  }

  @Override
  public void setPasswordTwo(String passwordTwo) {
    this.passwordTwo = passwordTwo;
  }
}
