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
package gov.idaho.isp.saktrack;

import gov.idaho.isp.saktrack.domain.user.AdminUser;
import gov.idaho.isp.saktrack.domain.user.User;
import gov.idaho.isp.saktrack.security.CustomInMemoryUserDetailsManager;
import gov.idaho.isp.saktrack.security.CustomWebAuthenticationDetails;
import jakarta.servlet.http.HttpServletRequest;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.authentication.AuthenticationDetailsSource;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.configurers.HeadersConfigurer;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.security.web.authentication.WebAuthenticationDetails;

import java.util.List;

@Configuration
@EnableWebSecurity
public class SecurityConfig {

  @Bean
  public SecurityFilterChain filterChain(HttpSecurity http, @Value("${spring.profiles.active}") String activeProfile) throws Exception {
    // when not requiring h2 console access remove this line:
    allowAdminAccessToH2Console(http);

    http.authorizeHttpRequests(a -> a
      .requestMatchers("/").permitAll()
      .requestMatchers("/WEB-INF/**", "/error").permitAll()
      .requestMatchers("/*/js/**").permitAll()
      .requestMatchers("/*/css/**").permitAll()
      .requestMatchers("/*/fonts/**").permitAll()
      .requestMatchers("/*/images/**").permitAll()
      .requestMatchers("/**favicon.ico").permitAll()
      .requestMatchers("/logout").permitAll()
      .requestMatchers("/timeline").permitAll()
      .requestMatchers("/register").permitAll()
      .requestMatchers("/reset/**").permitAll()
      .requestMatchers("/unsubscribe/**").permitAll()
      .requestMatchers("/passkey").authenticated()
      .requestMatchers("/manageAccount/**").authenticated()
      .requestMatchers("/organizations").hasAnyAuthority("ADMIN", "MEDICAL", "LAW_ENFORCEMENT", "LAB")
      .requestMatchers("/help/**").hasAnyAuthority("ADMIN", "MEDICAL", "LAW_ENFORCEMENT", "LAB", "LEGAL")
      .requestMatchers("/search/**").hasAnyAuthority("ADMIN", "MEDICAL", "LAW_ENFORCEMENT", "LAB", "LEGAL")
      .requestMatchers("/lab/**").hasAnyAuthority("ADMIN", "LAB")
      .requestMatchers("/medical/**").hasAnyAuthority("ADMIN", "MEDICAL")
      .requestMatchers("/law-enforcement/**").hasAnyAuthority("ADMIN", "LAW_ENFORCEMENT")
      .requestMatchers("/legal/**").hasAnyAuthority("ADMIN", "LEGAL")
      .requestMatchers("/organization/new", "/organization/save", "/organization/*/remove").hasAuthority("ADMIN")
      .requestMatchers("/organization/**").hasAnyAuthority("ADMIN", "ORG_ADMIN")
      .anyRequest().hasAuthority("ADMIN")
    );

    http.formLogin(form -> form.loginPage("/login").authenticationDetailsSource(getAuthenticationDetailsSource()).permitAll());
    http.logout(logout -> logout.logoutUrl("/logout").permitAll());

    if ("dev".equals(activeProfile)) {
      http.userDetailsService(buildInMemoryAuthUserDetailsManager());
    }
    return http.build();
  }

  private void allowAdminAccessToH2Console(HttpSecurity http) throws Exception {
    http.csrf(c ->  c.ignoringRequestMatchers("/h2-console/**"));
    http.authorizeHttpRequests(a -> a.requestMatchers("/seedDemoData").permitAll());
    http.headers(c -> c.frameOptions(HeadersConfigurer.FrameOptionsConfig::disable));
  }

  public CustomInMemoryUserDetailsManager buildInMemoryAuthUserDetailsManager() {
    AdminUser adminUser = new AdminUser();
    adminUser.setDisplayName("Administrator (in memory)");
    adminUser.setUserDetails(new org.springframework.security.core.userdetails.User("admin", getPasswordEncoder().encode("admin"), List.of(new SimpleGrantedAuthority(User.Type.ADMIN.toString()))));
    return new CustomInMemoryUserDetailsManager(adminUser);
  }

  private AuthenticationDetailsSource<HttpServletRequest, WebAuthenticationDetails> getAuthenticationDetailsSource() {
    return CustomWebAuthenticationDetails::new;
  }

  @Bean
  public PasswordEncoder getPasswordEncoder() {
    return new BCryptPasswordEncoder();
  }
}
