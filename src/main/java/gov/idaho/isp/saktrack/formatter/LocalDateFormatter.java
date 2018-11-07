package gov.idaho.isp.saktrack.formatter;

import java.text.ParseException;
import java.time.LocalDate;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;
import org.springframework.format.Formatter;

public class LocalDateFormatter implements Formatter<LocalDate> {
  private final String printFormat;
  private final List<String> parseFormats;

  public LocalDateFormatter(String format) {
    this(format, Arrays.asList(format));
  }

  public LocalDateFormatter(String printFormat, List<String> parseFormats) {
    this.printFormat = printFormat;
    this.parseFormats = parseFormats;
  }

  @Override
  public String print(LocalDate date, Locale locale) {
    return TimeUtils.print(date, printFormat, locale);
  }

  @Override
  public LocalDate parse(String string, Locale locale) throws ParseException {
    return TimeUtils.parseAsLocalDate(string, parseFormats, locale);
  }
}
