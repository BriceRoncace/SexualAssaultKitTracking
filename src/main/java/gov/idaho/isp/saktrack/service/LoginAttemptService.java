package gov.idaho.isp.saktrack.service;

import gov.idaho.isp.saktrack.util.CacheMap;
import gov.idaho.isp.saktrack.util.TimeValue;
import java.util.Optional;
import javax.servlet.http.HttpServletRequest;
import org.springframework.context.event.EventListener;
import org.springframework.security.authentication.event.AbstractAuthenticationEvent;
import org.springframework.security.authentication.event.AuthenticationFailureBadCredentialsEvent;
import org.springframework.security.authentication.event.AuthenticationSuccessEvent;
import org.springframework.security.core.AuthenticationException;
import org.springframework.stereotype.Service;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

@Service
public class LoginAttemptService {
  public enum Mode {
    BY_USERNAME {
      @Override
      public String getKey(AbstractAuthenticationEvent event) {
        return event.getAuthentication().getName();
      }
    },
    BY_IP {
      @Override
      public String getKey(AbstractAuthenticationEvent event) {
        return getClientIp();
      }
    };

    public abstract String getKey(AbstractAuthenticationEvent event);
  }

  private static final int MAX_DEFAULT = 10;
  private static final TimeValue TTL_DEFAULT = TimeValue.minutes(30);

  private final CacheMap<String, Integer> loginAttempts;
  private final int maxLoginAttempts;
  private final Mode mode;

  public LoginAttemptService() {
    this(Mode.BY_IP, MAX_DEFAULT, TTL_DEFAULT);
  }

  public LoginAttemptService(Mode mode) {
    this(mode, MAX_DEFAULT, TTL_DEFAULT);
  }

  public LoginAttemptService(Mode mode, int maxLoginAttempts, TimeValue cacheTimeToLive) {
    this.mode = mode;
    this.maxLoginAttempts = maxLoginAttempts;
    this.loginAttempts = new CacheMap<>(cacheTimeToLive);
  }

  public void loginSucceeded(String usernameOrIp) {
    usernameOrIp = normalize(usernameOrIp);
    loginAttempts.remove(usernameOrIp.toLowerCase().trim());
  }

  public void loginFailed(String usernameOrIp) {
    usernameOrIp = normalize(usernameOrIp);
    int attempts = loginAttempts.getOrDefault(usernameOrIp, 0);
    loginAttempts.put(usernameOrIp, ++attempts);
  }

  public boolean hasExceededMaxLoginAttempts(String usernameOrIp) {
    usernameOrIp = normalize(usernameOrIp);
    return loginAttempts.getOrDefault(usernameOrIp, 0) >= maxLoginAttempts;
  }

  public void assertHasNotExceededMaxLoginAttempts(String username) throws MaxLoginAttemptsExceededException {
    String usernameOrIp = mode == Mode.BY_IP ? getClientIp() : username;
    if (hasExceededMaxLoginAttempts(usernameOrIp)) {
      throw new MaxLoginAttemptsExceededException(usernameOrIp, this);
    }
  }

  private static String normalize(String str) {
    return str == null ? "" : str.trim().toLowerCase();
  }

  public static Optional<HttpServletRequest> getCurrentRequest() {
    return Optional.ofNullable(RequestContextHolder.getRequestAttributes())
      .filter(ServletRequestAttributes.class::isInstance)
      .map(ServletRequestAttributes.class::cast)
      .map(ServletRequestAttributes::getRequest);
  }

  public static String getClientIp() {
    return getCurrentRequest().map(req -> {
      String xfHeader = req.getHeader("X-Forwarded-For");
      return xfHeader == null ? req.getRemoteAddr() : xfHeader.split(",")[0];
    }).orElse("");
  }

  public Mode getMode() {
    return mode;
  }

  public int getMaxLoginAttempts() {
    return maxLoginAttempts;
  }

  public TimeValue getCacheTimeToLive() {
    return loginAttempts.getDefaultKeyExpiration();
  }

  @EventListener
  public void onApplicationEvent(AuthenticationFailureBadCredentialsEvent event) {
    loginFailed(mode.getKey(event));
  }

  @EventListener
  public void onApplicationEvent(AuthenticationSuccessEvent event) {
    loginSucceeded(mode.getKey(event));
  }

  public static class MaxLoginAttemptsExceededException extends AuthenticationException {
    private final String usernameOrIp;
    private final Mode mode;
    private final int maximumLoginAttempts;
    private final TimeValue lockoutTime;

    public MaxLoginAttemptsExceededException(String usernameOrIp, LoginAttemptService service) {
      super(buildErrorMessage(usernameOrIp, service));
      this.usernameOrIp = usernameOrIp;
      this.mode = service.getMode();
      this.maximumLoginAttempts = service.getMaxLoginAttempts();
      this.lockoutTime = service.getCacheTimeToLive();
    }

    private static String buildErrorMessage(String usernameOrIp, LoginAttemptService service) {
      String actor = service.getMode() == Mode.BY_IP ? "IP " + usernameOrIp : "User " + usernameOrIp;
      return String.format("[%s] has exceeded the maximumn allowed login attempts [%d]. [%s] will be locked for [%s]", actor, service.getMaxLoginAttempts(), actor, service.getCacheTimeToLive().prettyPrint());
    }

    public Mode getMode() {
      return mode;
    }

    public String getUsernameOrIp() {
      return usernameOrIp;
    }

    public int getMaximumLoginAttempts() {
      return maximumLoginAttempts;
    }

    public TimeValue getLockoutTime() {
      return lockoutTime;
    }
  }
}

