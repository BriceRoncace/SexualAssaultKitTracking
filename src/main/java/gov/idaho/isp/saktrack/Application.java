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

import gov.idaho.isp.saktrack.formatter.LocalDateFormatter;
import gov.idaho.isp.saktrack.hibernate.SpringAwareHibernateInterceptor;
import gov.idaho.isp.saktrack.mailer.Mailer;
import gov.idaho.isp.saktrack.mailer.SpringMailer;
import jakarta.validation.Validator;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.orm.jpa.HibernatePropertiesCustomizer;
import org.springframework.boot.web.servlet.support.SpringBootServletInitializer;
import org.springframework.context.annotation.Bean;
import org.springframework.format.Formatter;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.stereotype.Component;
import org.springframework.validation.beanvalidation.LocalValidatorFactoryBean;

import java.time.LocalDate;
import java.util.Arrays;
import java.util.Map;

@SpringBootApplication
@EnableAsync
public class Application extends SpringBootServletInitializer {

  public static void main(String[] args) throws Exception {
    SpringApplication.run(Application.class, args);
  }

  @Bean
  public Formatter<LocalDate> getLocalDateFormatter(@Value("${date.format.print:MM/dd/yyyy}") String printFormat, @Value("${date.format.parse:MM/dd/yyyy}") String parseFormats) {
    return new LocalDateFormatter(printFormat, Arrays.asList(parseFormats.split(",")));
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
  static class MyHibernateInterceptorCustomizer implements HibernatePropertiesCustomizer {
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
