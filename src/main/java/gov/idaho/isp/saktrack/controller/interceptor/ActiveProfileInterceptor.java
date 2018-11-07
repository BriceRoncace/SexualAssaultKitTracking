package gov.idaho.isp.saktrack.controller.interceptor;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.web.servlet.handler.HandlerInterceptorAdapter;

@Component
public class ActiveProfileInterceptor extends HandlerInterceptorAdapter {
  private final String activeProfile;

  public ActiveProfileInterceptor(@Value("${spring.profiles.active}") String activeProfile) {
    this.activeProfile = activeProfile;
  }

  @Override
  public boolean preHandle(HttpServletRequest req, HttpServletResponse res, Object handler) throws Exception {
    req.setAttribute("activeProfile", activeProfile);
    return true;
  }
}
