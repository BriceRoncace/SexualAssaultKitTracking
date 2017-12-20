package gov.idaho.isp.saktrack.controller.interceptor;

import java.time.LocalDate;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.web.servlet.handler.HandlerInterceptorAdapter;

@Component
public class DateFormatInterceptor extends HandlerInterceptorAdapter {
  @Value(value = "${date.format}")
  private String dateFormat;
  @Value(value = "${date.time.format}")
  private String dateTimeFormat;

  @Override
  public boolean preHandle(HttpServletRequest request, HttpServletResponse response, Object handler) throws Exception {
    request.setAttribute("dateFormat", dateFormat);
    request.setAttribute("dateTimeFormat", dateTimeFormat);
    request.setAttribute("currentDate", LocalDate.now());
    return true;
  }

  public String getDateFormat() {
    return dateFormat;
  }

  public String getDateTimeFormat() {
    return dateTimeFormat;
  }
}