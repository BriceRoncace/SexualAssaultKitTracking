package gov.idaho.isp.saktrack.security;

import javax.servlet.http.HttpServletRequest;
import org.springframework.security.web.authentication.WebAuthenticationDetails;

public class CustomWebAuthenticationDetails extends WebAuthenticationDetails {
  private final String userAgent;

  public CustomWebAuthenticationDetails(HttpServletRequest req) {
    super(req);
    this.userAgent = req.getHeader("User-Agent");
  }

  public String getUserAgent() {
    return userAgent;
  }

  @Override
  public String toString() {
    return "CustomWebAuthenticationDetails{" + "userAgent=" + userAgent + ", " + super.toString() + '}';
  }
}
