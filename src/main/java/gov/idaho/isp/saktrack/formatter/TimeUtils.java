package gov.idaho.isp.saktrack.formatter;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.time.temporal.TemporalAccessor;
import java.util.List;
import java.util.Locale;
import java.util.function.BiFunction;

public class TimeUtils {
  public static String print(TemporalAccessor ta, String format, Locale locale) {
    return DateTimeFormatter.ofPattern(format, locale).format(ta);
  }

  public static LocalDate parseAsLocalDate(String value, List<String> formats, Locale locale) {
    return parse(value, formats, locale, (str, formatter) -> LocalDate.parse(str, formatter));
  }

  public static LocalDateTime parseAsLocalDateTime(String value, List<String> formats, Locale locale) {
    return parse(value, formats, locale, (str, formatter) -> LocalDateTime.parse(str, formatter));
  }

  private static <T> T parse(String value, List<String> formats, Locale locale, BiFunction<String, DateTimeFormatter, T> parseFunction) {
    T parsed = null;
    for (String format : formats) {
      try {
        parsed = parseFunction.apply(value, DateTimeFormatter.ofPattern(format, locale));
      }
      catch (Exception ignore) {
      }
    }

    if (parsed == null) {
      throw new NullPointerException(String.format("Could not parse string \"%s\" using formats %s", value, formats));
    }
    return parsed;
  }
}
