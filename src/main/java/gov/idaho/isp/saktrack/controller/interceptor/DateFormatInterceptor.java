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

package gov.idaho.isp.saktrack.controller.interceptor;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.web.servlet.HandlerInterceptor;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

@Component
public class DateFormatInterceptor implements HandlerInterceptor {
  private final String dateFormat;
  private final String dateTimeFormat;

  public DateFormatInterceptor(@Value("${date.format}") String dateFormat, @Value("${date.time.format}") String dateTimeFormat) {
    this.dateFormat = dateFormat;
    this.dateTimeFormat = dateTimeFormat;
  }

  @Override
  public boolean preHandle(HttpServletRequest req, HttpServletResponse res, Object handler) throws Exception {
    req.setAttribute("dateFormat", dateFormat);
    req.setAttribute("dateFormatter", new NullSafeLocalDateFormatter(DateTimeFormatter.ofPattern(dateFormat)));

    req.setAttribute("dateTimeFormat", dateTimeFormat);
    req.setAttribute("dateTimeFormatter", new NullSafeLocalDateTimeFormatter(DateTimeFormatter.ofPattern(dateTimeFormat)));

    req.setAttribute("currentDate", LocalDate.now());
    return true;
  }

  public String getDateFormat() {
    return dateFormat;
  }

  public String getDateTimeFormat() {
    return dateTimeFormat;
  }

  public class NullSafeLocalDateFormatter {
    private final DateTimeFormatter formatter;

    public NullSafeLocalDateFormatter(DateTimeFormatter formatter) {
      this.formatter = formatter;
    }

    public String format(LocalDate ld) {
      return ld != null ? formatter.format(ld) : "";
    }
  }

  public class NullSafeLocalDateTimeFormatter {
    private final DateTimeFormatter formatter;

    public NullSafeLocalDateTimeFormatter(DateTimeFormatter formatter) {
      this.formatter = formatter;
    }

    public String format(LocalDateTime ldt) {
      return ldt != null ? formatter.format(ldt) : "";
    }
  }
}