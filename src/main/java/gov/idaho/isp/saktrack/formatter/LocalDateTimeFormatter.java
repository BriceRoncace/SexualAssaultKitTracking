/* 
 * Copyright 2017 Idaho State Police.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

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
