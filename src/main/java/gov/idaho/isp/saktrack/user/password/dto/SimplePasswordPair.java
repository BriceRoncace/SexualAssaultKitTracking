package gov.idaho.isp.saktrack.user.password.dto;

import gov.idaho.isp.saktrack.validation.PasswordPolicy;
import gov.idaho.isp.saktrack.validation.PropertiesEqual;

@PropertiesEqual(propertyNameOne = "passwordOne", propertyNameTwo = "passwordTwo", message = "{passwords.do.not.match}")
public class SimplePasswordPair implements PasswordPair {
  @PasswordPolicy(minSize = 8, capitals = 1, numbers = 1, specials = 1)
  private String passwordOne;

  private String passwordTwo;

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

  @Override
  public String toString() {
    return "SimplePasswordPair{" + "passwordOne=" + passwordOne + ", passwordTwo=" + passwordTwo + '}';
  }
}
