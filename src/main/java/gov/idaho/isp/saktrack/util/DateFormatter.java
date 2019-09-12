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

package gov.idaho.isp.saktrack.util;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

@Component
public class DateFormatter {
  private static final String DEFAULT_DATE_FORMAT = "MM/dd/yyyy";
  private static final String DEFAULT_DATE_TIME_FORMAT = "MM/dd/yyyy HHmm";

  private static DateTimeFormatter DATE_FORMATTER = DateTimeFormatter.ofPattern(DEFAULT_DATE_FORMAT);
  private static DateTimeFormatter DATE_TIME_FORMATTER = DateTimeFormatter.ofPattern(DEFAULT_DATE_TIME_FORMAT);

  public static String format(LocalDate localDate) {
    return localDate != null ? localDate.format(DATE_FORMATTER) : null;
  }

  public static String format(LocalDateTime localDateTime) {
    return localDateTime != null ? localDateTime.format(DATE_TIME_FORMATTER) : null;
  }

  public static String formatDateOnly(LocalDateTime localDateTime) {
    return localDateTime != null ? localDateTime.format(DATE_FORMATTER) : null;
  }

  @Value("${date.format}")
  public void setDateFormat(String dateFormat) {
    DateFormatter.DATE_FORMATTER = DateTimeFormatter.ofPattern(dateFormat);
  }

  @Value("${date.time.format}")
  public void setDateTimeFormat(String dateTimeFormat) {
    DateFormatter.DATE_TIME_FORMATTER = DateTimeFormatter.ofPattern(dateTimeFormat);
  }
}