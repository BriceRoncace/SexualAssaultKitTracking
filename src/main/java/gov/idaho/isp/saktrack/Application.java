package gov.idaho.isp.saktrack;

import gov.idaho.isp.saktrack.formatter.LocalDateFormatter;
import gov.idaho.isp.saktrack.hibernate.SpringAwareHibernateInterceptor;
import gov.idaho.isp.saktrack.mailer.Mailer;
import gov.idaho.isp.saktrack.mailer.SpringMailer;
import java.time.LocalDate;
import java.util.Arrays;
import java.util.Map;
import javax.validation.Validator;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.orm.jpa.HibernatePropertiesCustomizer;
import org.springframework.boot.web.servlet.support.SpringBootServletInitializer;
import org.springframework.context.annotation.Bean;
import org.springframework.format.Formatter;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.stereotype.Component;
import org.springframework.validation.beanvalidation.LocalValidatorFactoryBean;

@SpringBootApplication
public class Application extends SpringBootServletInitializer {

  public static void main(String[] args) throws Exception {
    SpringApplication.run(Application.class, args);
  }

  @Bean
  public Formatter<LocalDate> getLocalDateFormatter(@Value("${date.format.print:MM/dd/yyyy}") String printFormat, @Value("${date.format.parse:MM/dd/yyyy}") String parseFormat) {
    return new LocalDateFormatter(printFormat, Arrays.asList(parseFormat));
  }

  @Bean
  public Mailer getMailer(JavaMailSender javaMailSender, @Value("${mail.from}") String defaultSender, @Value("${mail.test.mode}") boolean testMode, @Value("${mail.test.to}") String testDestinations) {
    SpringMailer mailer = new SpringMailer(javaMailSender);
    mailer.setDefaultSender(defaultSender);
    mailer.setInTestMode(testMode);
    mailer.setTestDestinations(testDestinations);
    return mailer;
  }

  @Bean
  public Validator getLocalValidatorFactoryBean() {
    return new LocalValidatorFactoryBean();
  }

  @Component
  class MyHibernateInterceptorCustomizer implements HibernatePropertiesCustomizer {
    private final SpringAwareHibernateInterceptor springAwareHibernateInterceptor;

    public MyHibernateInterceptorCustomizer(SpringAwareHibernateInterceptor springAwareHibernateInterceptor) {
      this.springAwareHibernateInterceptor = springAwareHibernateInterceptor;
    }

    @Override
    public void customize(Map<String, Object> hibernateProperties) {
      hibernateProperties.put("hibernate.session_factory.interceptor", springAwareHibernateInterceptor);
    }
  }
}
