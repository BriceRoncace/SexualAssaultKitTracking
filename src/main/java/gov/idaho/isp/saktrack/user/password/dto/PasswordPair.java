package gov.idaho.isp.saktrack.user.password.dto;

public interface PasswordPair {
  String getPasswordOne();
  void setPasswordOne(String password);
  
  String getPasswordTwo();
  void setPasswordTwo(String password);
}