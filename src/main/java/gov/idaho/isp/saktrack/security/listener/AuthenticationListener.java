package gov.idaho.isp.saktrack.security.listener;

import gov.idaho.isp.saktrack.domain.user.User;
import gov.idaho.isp.saktrack.domain.user.UserLogin;
import gov.idaho.isp.saktrack.domain.user.UserLoginRepository;
import gov.idaho.isp.saktrack.security.CustomWebAuthenticationDetails;
import java.util.Optional;
import org.springframework.context.ApplicationListener;
import org.springframework.security.authentication.event.InteractiveAuthenticationSuccessEvent;
import org.springframework.stereotype.Component;


@Component
public class AuthenticationListener implements ApplicationListener<InteractiveAuthenticationSuccessEvent> {
  private final UserLoginRepository userLoginRepository;

  public AuthenticationListener(UserLoginRepository userLoginRepository) {
    this.userLoginRepository = userLoginRepository;
  }

  @Override
  public void onApplicationEvent(InteractiveAuthenticationSuccessEvent e) {
    userLoginRepository.save(toUserLogin(e));
  }

  private UserLogin toUserLogin(InteractiveAuthenticationSuccessEvent e) {
    return new UserLogin(getUser(e), getUserAgent(e).orElse(null));
  }

  private User getUser(InteractiveAuthenticationSuccessEvent e) {
    if (e.getAuthentication().getPrincipal() instanceof User) {
      return (User) e.getAuthentication().getPrincipal();
    }
    throw new IllegalStateException("Authentiation's Principal should be instance of User");
  }

  private Optional<String> getUserAgent(InteractiveAuthenticationSuccessEvent e) {
    Object authDetails = e.getAuthentication().getDetails();
    if (authDetails instanceof CustomWebAuthenticationDetails) {
      return Optional.of(((CustomWebAuthenticationDetails) authDetails).getUserAgent());
    }
    return Optional.empty();
  }
}
