package gov.idaho.isp.saktrack.config;

import gov.idaho.isp.saktrack.security.CustomDatabaseUserDetailsService;
import gov.idaho.isp.saktrack.security.CustomInMemoryUserDetailsManager;
import gov.idaho.isp.saktrack.security.CustomLdapUserDetailsMapper;
import gov.idaho.isp.saktrack.user.AdminUser;
import gov.idaho.isp.saktrack.user.User;
import java.util.Arrays;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.ldap.core.support.LdapContextSource;
import org.springframework.security.authentication.dao.DaoAuthenticationProvider;
import org.springframework.security.config.annotation.authentication.builders.AuthenticationManagerBuilder;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.configuration.WebSecurityConfigurerAdapter;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.security.web.util.matcher.AntPathRequestMatcher;

@EnableWebSecurity
public class SecurityConfig extends WebSecurityConfigurerAdapter {

  @Autowired
  private CustomDatabaseUserDetailsService customDatabaseUserDetailsService;

  @Autowired
  private CustomLdapUserDetailsMapper customLdapUserDetailsMapper;

  @Value("${ldap.url}")
  private String ldapUrl;

  @Value("${ldap.base}")
  private String ldapBase;

  @Value("${ldap.user.dn}")
  private String ldapUserDn;

  @Value("${ldap.user.password}")
  private String ldapUserPassword;

  @Value("${ldap.auth.user.search.base}")
  private String ldapUserSearchBase;

  @Value("${ldap.auth.user.search.filter}")
  private String ldapUserSearchFilter;

  @Value("${ldap.auth.group.search.base}")
  private String ldapGroupSearchBase;

  @Value("${ldap.auth.group.search.filter}")
  private String ldapGroupSearchFilter;

  @Autowired
  public void configureGlobal(AuthenticationManagerBuilder auth, @Value("${ldap.auth.enabled}") Boolean configureLdap, @Value("${test.mode}") Boolean testMode) throws Exception {
    if (Boolean.TRUE.equals(configureLdap)) {
      configureLdapAuthentication(auth);
    }

    configureDbAuthentication(auth);

    if (Boolean.TRUE.equals(testMode)) {
      configureInMemoryAuthentication(auth);
    }
  }

  private void configureLdapAuthentication(AuthenticationManagerBuilder auth) throws Exception {
    auth.ldapAuthentication()
      .userSearchBase(ldapUserSearchBase)
      .userSearchFilter(ldapUserSearchFilter)
      .groupSearchBase(ldapGroupSearchBase)
      .groupSearchFilter(ldapGroupSearchFilter)
      .contextSource(getContextSource())
      .rolePrefix("")
      .userDetailsContextMapper(customLdapUserDetailsMapper);
  }

  private void configureDbAuthentication(AuthenticationManagerBuilder auth) {
    auth.authenticationProvider(getDaoAuthProvider());
  }

  private void configureInMemoryAuthentication(AuthenticationManagerBuilder auth) throws Exception {
    AdminUser adminUser = new AdminUser(User.AuthMethod.IN_MEMORY);
    adminUser.setUserDetails(new org.springframework.security.core.userdetails.User("admin", "admin", Arrays.asList(new SimpleGrantedAuthority(User.Type.ADMIN.toString()))));
    auth.userDetailsService(new CustomInMemoryUserDetailsManager(adminUser));
  }

  @Bean
  public DaoAuthenticationProvider getDaoAuthProvider() {
    DaoAuthenticationProvider provider = new DaoAuthenticationProvider();
    provider.setUserDetailsService(customDatabaseUserDetailsService);
    provider.setPasswordEncoder(getPasswordEncoder());
    return provider;
  }

  @Bean
  public PasswordEncoder getPasswordEncoder() {
    return new BCryptPasswordEncoder();
  }

  @Bean
  public LdapContextSource getContextSource() {
    LdapContextSource cs = new LdapContextSource();
    cs.setUrl(ldapUrl);
    cs.setBase(ldapBase);
    cs.setUserDn(ldapUserDn);
    cs.setPassword(ldapUserPassword);
    cs.afterPropertiesSet();
    return cs;
  }

  @Override
  protected void configure(HttpSecurity http) throws Exception {
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
      .antMatchers("/ldapUser/**").hasAnyAuthority("ADMIN", "ORG_ADMIN")
      .anyRequest().hasAuthority("ADMIN")
      .and().formLogin().loginPage("/login").permitAll().and().logout().permitAll();

    http.logout().logoutRequestMatcher(new AntPathRequestMatcher("/logout"));
  }
}
