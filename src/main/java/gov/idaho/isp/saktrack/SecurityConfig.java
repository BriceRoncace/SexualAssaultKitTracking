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
import gov.idaho.isp.saktrack.security.CustomDatabaseUserDetailsService;
import gov.idaho.isp.saktrack.security.CustomInMemoryUserDetailsManager;
import gov.idaho.isp.saktrack.security.CustomWebAuthenticationDetails;
import java.util.Arrays;
import javax.servlet.http.HttpServletRequest;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.authentication.AuthenticationDetailsSource;
import org.springframework.security.authentication.dao.DaoAuthenticationProvider;
import org.springframework.security.config.annotation.authentication.builders.AuthenticationManagerBuilder;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.configuration.WebSecurityConfigurerAdapter;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.security.web.authentication.WebAuthenticationDetails;
import org.springframework.security.web.util.matcher.AntPathRequestMatcher;

@Configuration
@EnableWebSecurity
public class SecurityConfig extends WebSecurityConfigurerAdapter {

  @Override
  protected void configure(HttpSecurity http) throws Exception {
    // when not requiring h2 console access remove this line:
    allowAdminAccessToH2Console(http);
    
    http.authorizeRequests()
      .antMatchers("/").permitAll()
      .antMatchers("/**/js/**").permitAll()
      .antMatchers("/**/css/**").permitAll()
      .antMatchers("/**/fonts/**").permitAll()
      .antMatchers("/**/images/**").permitAll()
      .antMatchers("/**/favicon.ico").permitAll()
      .antMatchers("/logout").permitAll()
      .antMatchers("/timeline").permitAll()
      .antMatchers("/register").permitAll()
      .antMatchers("/reset/**").permitAll()
      .antMatchers("/unsubscribe/**").permitAll()
      .antMatchers("/passkey").authenticated()
      .antMatchers("/manageAccount/**").authenticated()
      .antMatchers("/organizations").hasAnyAuthority("ADMIN", "MEDICAL", "LAW_ENFORCEMENT", "LAB")
      .antMatchers("/help/**").hasAnyAuthority("ADMIN", "MEDICAL", "LAW_ENFORCEMENT", "LAB", "LEGAL")
      .antMatchers("/search/**").hasAnyAuthority("ADMIN", "MEDICAL", "LAW_ENFORCEMENT", "LAB", "LEGAL")
      .antMatchers("/lab/**").hasAnyAuthority("ADMIN", "LAB")
      .antMatchers("/medical/**").hasAnyAuthority("ADMIN", "MEDICAL")
      .antMatchers("/law-enforcement/**").hasAnyAuthority("ADMIN", "LAW_ENFORCEMENT")
      .antMatchers("/legal/**").hasAnyAuthority("ADMIN", "LEGAL")
      .antMatchers("/organization/new", "organization/save", "/organization/*/remove").hasAuthority("ADMIN")
      .antMatchers("/organization/**").hasAnyAuthority("ADMIN", "ORG_ADMIN")
      .anyRequest().hasAuthority("ADMIN")
      .and().formLogin().loginPage("/login").authenticationDetailsSource(getAuthenticationDetailsSource()).permitAll()
      .and().logout().permitAll();

    http.logout().logoutRequestMatcher(new AntPathRequestMatcher("/logout"));
  }

  private void allowAdminAccessToH2Console(HttpSecurity http) throws Exception {
    http.csrf().ignoringAntMatchers("/h2-console/**");
    http.authorizeRequests().antMatchers("/seedDemoData").permitAll();
    http.headers().frameOptions().disable();
  }

  @Autowired
  public void configureGlobal(AuthenticationManagerBuilder auth, @Value("${spring.profiles.active}") String activeProfile, CustomDatabaseUserDetailsService customDatabaseUserDetailsService) throws Exception {
    configureDbAuthentication(auth, customDatabaseUserDetailsService);
    if ("dev".equals(activeProfile)) {
      configureInMemoryAuthentication(auth);
    }
  }

  private void configureDbAuthentication(AuthenticationManagerBuilder auth, CustomDatabaseUserDetailsService customDatabaseUserDetailsService) {
    auth.authenticationProvider(getDaoAuthProvider(customDatabaseUserDetailsService));
  }

  private void configureInMemoryAuthentication(AuthenticationManagerBuilder auth) throws Exception {
    AdminUser adminUser = new AdminUser();
    adminUser.setDisplayName("Administrator (in memory)");
    adminUser.setUserDetails(new org.springframework.security.core.userdetails.User("admin", getPasswordEncoder().encode("admin"), Arrays.asList(new SimpleGrantedAuthority(User.Type.ADMIN.toString()))));
    auth.userDetailsService(new CustomInMemoryUserDetailsManager(adminUser));
  }

  @Bean
  public DaoAuthenticationProvider getDaoAuthProvider(CustomDatabaseUserDetailsService customDatabaseUserDetailsService ) {
    DaoAuthenticationProvider provider = new DaoAuthenticationProvider();
    provider.setUserDetailsService(customDatabaseUserDetailsService);
    provider.setPasswordEncoder(getPasswordEncoder());
    return provider;
  }

  private AuthenticationDetailsSource<HttpServletRequest, WebAuthenticationDetails> getAuthenticationDetailsSource() {
    return (HttpServletRequest req) -> new CustomWebAuthenticationDetails(req);
  }

  @Bean
  public PasswordEncoder getPasswordEncoder() {
    return new BCryptPasswordEncoder();
  }
}
