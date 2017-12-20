package gov.idaho.isp.saktrack.service;

import gov.idaho.isp.saktrack.user.password.dto.ResetPasswordPair;

public interface PasswordResetService {
  String requestReset(String username);
  boolean isRequestValid(String request);
  boolean resetPassword(ResetPasswordPair passwordPair);
}
