package gov.idaho.isp.saktrack.exception;

public class UnexpectedUserException extends SexualAssaultKitTrackingException {

  public UnexpectedUserException(String message, String... errors) {
    this(message, null, errors);
  }

  public UnexpectedUserException(String message, Throwable cause, String... errors) {
    super(message, cause, errors);
  }
}