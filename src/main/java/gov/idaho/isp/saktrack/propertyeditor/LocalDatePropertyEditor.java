package gov.idaho.isp.saktrack.propertyeditor;

import java.beans.PropertyEditorSupport;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.Arrays;
import java.util.List;
import org.springframework.util.StringUtils;

public class LocalDatePropertyEditor extends PropertyEditorSupport {
  private final List<String> dateFormats;

  public LocalDatePropertyEditor(String... dateFormats) {
    this.dateFormats = Arrays.asList(dateFormats);
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
      LocalDate d = (LocalDate) getValue();
      return d.format(getDefaultDateTimeFormatter());
    }
    return null;
  }

  private DateTimeFormatter getDefaultDateTimeFormatter() {
    return !dateFormats.isEmpty() ? DateTimeFormatter.ofPattern(dateFormats.get(0)) : DateTimeFormatter.ISO_DATE;
  }

  private LocalDate format(String value) {
    LocalDate localDate = null;
    RuntimeException parseException = null;
    for (String dateFormat : dateFormats) {
      try {
        localDate = LocalDate.parse(value, DateTimeFormatter.ofPattern(dateFormat));
      }
      catch (RuntimeException ex) {
        parseException = ex;
      }
    }

    if (localDate == null && parseException != null) {
      throw parseException;
    }

    return localDate;
  }

  public List<String> getDateFormats() {
    return dateFormats;
  }

  @Override
  public String toString() {
    return "LocalDatePropertyEditor{" + "dateFormats=" + dateFormats + '}';
  }
}