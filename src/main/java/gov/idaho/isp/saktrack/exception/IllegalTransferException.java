package gov.idaho.isp.saktrack.exception;

public class IllegalTransferException extends SexualAssaultKitTrackingException {

  public IllegalTransferException(String message, String... errors) {
    super(message, errors);
  }

  public IllegalTransferException(String message, Throwable cause, String... errors) {
    super(message, cause, errors);
  }
}
