package gov.idaho.isp.saktrack.exception;

public class NullSexualAssaultKitException extends SexualAssaultKitTrackingException {

  public NullSexualAssaultKitException(String message, String... errors) {
    super(message, errors);
  }

  public NullSexualAssaultKitException(String message, Throwable cause, String... errors) {
    super(message, cause, errors);
  }
}
