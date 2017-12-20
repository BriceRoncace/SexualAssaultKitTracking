package gov.idaho.isp.saktrack.exception;

import java.util.Set;

public interface ErrorAware {
  Set<String> getErrors();
}
