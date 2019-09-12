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

package gov.idaho.isp.saktrack.security;

import java.util.HashMap;
import java.util.Map;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.security.provisioning.UserDetailsManager;

public final class CustomInMemoryUserDetailsManager implements UserDetailsManager {
  private final Map<String,UserDetails> users = new HashMap<>();

  public CustomInMemoryUserDetailsManager(UserDetails... users) {
    for (UserDetails user : users) {
      createUser(user);
    }
  }

  @Override
  public void createUser(UserDetails user) {
    users.put(user.getUsername().toLowerCase(), user);
  }

  @Override
  public void deleteUser(String username) {
    users.remove(username.toLowerCase());
  }

  @Override
  public void updateUser(UserDetails user) {
    users.put(user.getUsername().toLowerCase(), user);
  }

  @Override
  public boolean userExists(String username) {
    return users.containsKey(username.toLowerCase());
  }

  @Override
  public void changePassword(String oldPassword, String newPassword) {
    throw new IllegalStateException("Cannot change in memory user's password.");
  }

  @Override
  public UserDetails loadUserByUsername(String username) throws UsernameNotFoundException {
    UserDetails user = users.get(username.toLowerCase());

    if (user == null) {
      throw new UsernameNotFoundException(username);
    }

    return user;
  }
}
