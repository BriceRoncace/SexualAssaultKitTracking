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
