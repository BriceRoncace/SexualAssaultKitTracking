package gov.idaho.isp.saktrack.formatter;

import java.text.ParseException;
import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;
import org.springframework.format.Formatter;

public class LocalDateTimeFormatter implements Formatter<LocalDateTime> {
  private final String printFormat;
  private final List<String> parseFormats;

  public LocalDateTimeFormatter(String format) {
    this(format, Arrays.asList(format));
  }

  public LocalDateTimeFormatter(String printFormat, List<String> parseFormats) {
    this.printFormat = printFormat;
    this.parseFormats = parseFormats;
  }

  @Override
  public String print(LocalDateTime dateTime, Locale locale) {
    return TimeUtils.print(dateTime, printFormat, locale);
  }

  @Override
  public LocalDateTime parse(String string, Locale locale) throws ParseException {
    return TimeUtils.parseAsLocalDateTime(string, parseFormats, locale);
  }
}
