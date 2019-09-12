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
