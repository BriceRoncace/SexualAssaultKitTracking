package gov.idaho.isp.saktrack.propertyeditor;

import java.beans.PropertyEditorSupport;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Arrays;
import java.util.List;
import org.springframework.util.StringUtils;

public class LocalDateTimePropertyEditor extends PropertyEditorSupport {
  private final List<String> dateTimeFormats;

  public LocalDateTimePropertyEditor(String... dateTimeFormat) {
    this.dateTimeFormats = Arrays.asList(dateTimeFormat);
  }

  @Override
  public void setAsText(String value) {
    if (!StringUtils.hasText(value)) {
      setValue(null);
    }
    else {
      setValue(format(value));
    }
  }

  @Override
  public String getAsText() {
    if (getValue() != null) {
      LocalDateTime dt = (LocalDateTime) getValue();
      return dt.format(getDefaultDateTimeFormatter());
    }
    return null;
  }

  private DateTimeFormatter getDefaultDateTimeFormatter() {
    return !dateTimeFormats.isEmpty() ? DateTimeFormatter.ofPattern(dateTimeFormats.get(0)) : DateTimeFormatter.ISO_DATE;
  }

  private LocalDateTime format(String value) {
    LocalDateTime localDateTime = null;
    RuntimeException parseException = null;
    for (String dateTimeFormat : dateTimeFormats) {
      try {
        localDateTime = LocalDateTime.parse(value, DateTimeFormatter.ofPattern(dateTimeFormat));
      }
      catch (RuntimeException ex) {
        parseException = ex;
      }
    }

    if (localDateTime == null && parseException != null) {
      throw parseException;
    }

    return localDateTime;
  }

  public List<String> getDateTimeFormats() {
    return dateTimeFormats;
  }

  @Override
  public String toString() {
    return "LocalDateTimePropertyEditor{" + "dateTimeFormats=" + dateTimeFormats + '}';
  }
}