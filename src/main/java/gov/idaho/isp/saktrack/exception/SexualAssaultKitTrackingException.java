package gov.idaho.isp.saktrack.exception;

import gov.idaho.isp.saktrack.util.collection.SetUtils;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

public class SexualAssaultKitTrackingException extends RuntimeException implements ErrorAware {
  private final Set<String> errors;

  public SexualAssaultKitTrackingException(String message, Throwable cause, Set<String> errors) {
    super(message, cause);
    this.errors = errors;
  }

  public SexualAssaultKitTrackingException(String message, Set<String> errors) {
    this(message, null, errors);
  }

  public SexualAssaultKitTrackingException(String message, String... errors) {
    this(message, null, errors);
  }

  public SexualAssaultKitTrackingException(String message, Throwable cause, String... errors) {
    this(message, cause, (errors != null && errors.length > 0 ? SetUtils.of(errors) : SetUtils.of(message)));
  }

  @Override
  public Set<String> getErrors() {
    if (errors == null || errors.isEmpty()) {
      return new HashSet<>(Arrays.asList(getMessage()));
    }
    return errors;
  }
}