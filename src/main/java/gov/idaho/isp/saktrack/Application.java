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
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.orm.jpa.HibernatePropertiesCustomizer;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.boot.web.servlet.support.SpringBootServletInitializer;
import org.springframework.context.annotation.Bean;
import org.springframework.format.Formatter;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.stereotype.Component;

import java.time.LocalDate;
import java.util.Arrays;
import java.util.Map;

@SpringBootApplication
@EnableAsync
@EnableConfigurationProperties(Application.PasswordProperties.class)
public class Application extends SpringBootServletInitializer {

  public static void main(String[] args) {
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

  @ConfigurationProperties(prefix = "password")
  public static class PasswordProperties {
    private int minLength = 8;
    private int maxLength = Integer.MAX_VALUE;
    private int capitals = 1;
    private int digits = 1;
    private int specials = 1;

    public int getMinLength() {
      return minLength;
    }

    public void setMinLength(int minLength) {
      this.minLength = minLength;
    }

    public int getMaxLength() {
      return maxLength;
    }

    public void setMaxLength(int maxLength) {
      this.maxLength = maxLength;
    }

    public int getCapitals() {
      return capitals;
    }

    public void setCapitals(int capitals) {
      this.capitals = capitals;
    }

    public int getDigits() {
      return digits;
    }

    public void setDigits(int digits) {
      this.digits = digits;
    }

    public int getSpecials() {
      return specials;
    }

    public void setSpecials(int specials) {
      this.specials = specials;
    }

    public boolean isEmpty() {
      return minLength == 0 && maxLength == Integer.MAX_VALUE && capitals == 0 && digits == 0 && specials == 0;
    }

    public String getPasswordPolicy() {
      return "Passwords must be %d or more characters long and contain at least %d capital letter(s), %d digit(s), and %d special character(s).".formatted(minLength, capitals, digits, specials);
    }

    @Override
    public String toString() {
      return "PasswordProperties{" +
        "minLength=" + minLength +
        ", capitals=" + capitals +
        ", digits=" + digits +
        ", specials=" + specials +
        '}';
    }
  }

}
