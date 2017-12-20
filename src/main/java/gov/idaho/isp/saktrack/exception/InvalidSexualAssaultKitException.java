package gov.idaho.isp.saktrack.exception;

import java.util.Set;

public class InvalidSexualAssaultKitException extends SexualAssaultKitTrackingException {
  public InvalidSexualAssaultKitException(String message, String... errors) {
    super(message, errors);
  }

  public InvalidSexualAssaultKitException(String message, Set<String> errors) {
    super(message, errors);
  }
}
