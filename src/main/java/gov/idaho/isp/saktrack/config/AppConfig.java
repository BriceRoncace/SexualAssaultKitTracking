package gov.idaho.isp.saktrack.config;

import gov.idaho.isp.ldap.user.LdapUserConfiguration;
import gov.idaho.isp.ldap.user.LdapUserDirectory;
import gov.idaho.isp.ldap.user.LdapUserRepositoryImpl;
import gov.idaho.isp.mail.MailMessage;
import gov.idaho.isp.mail.Mailer;
import gov.idaho.isp.mail.TestAwareMailer;
import java.util.List;
import javax.validation.Validator;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;
import org.springframework.ldap.core.ContextSource;
import org.springframework.ldap.core.LdapTemplate;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.validation.beanvalidation.LocalValidatorFactoryBean;

@Configuration
@EnableAsync
@ComponentScan(basePackages = {"gov.idaho.isp.saktrack", "gov.idaho.isp.cjis.hibernate"})
@Import(value = {PersistenceConfig.class, SecurityConfig.class, ScheduleConfig.class})
public class AppConfig {

  @Value("${use.test.mailer}")
  private Boolean useTestMailer;

  @Value("#{'${test.destinations}'.split(',')}")
  private List<String> testDestinations;

  @Bean
  public Mailer getMailer(@Value("${mail.host}") String host, @Value("${mail.from}") String from) {
    TestAwareMailer mailer = new TestAwareMailer();
    mailer.setHost(host);
    mailer.setDefaultSender(from);
    mailer.setDefaultMimeType(MailMessage.MimeType.HTML);
    mailer.setInTestMode(useTestMailer);
    mailer.setTestDestinations(testDestinations);
    return mailer;
  }

  @Bean
  public LdapUserDirectory getLdapUserDiretory(LdapTemplate ldapTemplate) {
    LdapUserRepositoryImpl userRepo = new LdapUserRepositoryImpl();
    userRepo.setLdapTemplate(ldapTemplate);
    userRepo.setLdapUserConfiguration(getLdapUserConfig());
    return userRepo;
  }

  @Bean
  public LdapTemplate getLdapTemplate(ContextSource contextSource) {
    return new LdapTemplate(contextSource);
  }

  private LdapUserConfiguration getLdapUserConfig() {
    return new LdapUserConfiguration().excludeAll()
      .setIncludeUsername(true)
      .setIncludeFirstName(true)
      .setIncludeMiddleInitial(true)
      .setIncludeLastName(true)
      .setIncludePhone(true)
      .setIncludeEmail(true);
  }

  @Bean
  public Validator getLocalValidatorFactoryBean() {
    return new LocalValidatorFactoryBean();
  }
}
