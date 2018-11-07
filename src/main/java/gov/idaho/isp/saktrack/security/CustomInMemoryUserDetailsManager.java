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
