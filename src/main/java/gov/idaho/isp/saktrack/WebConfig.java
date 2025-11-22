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

import gov.idaho.isp.saktrack.controller.interceptor.DateFormatInterceptor;
import gov.idaho.isp.saktrack.controller.interceptor.FlashForwardInterceptor;
import gov.idaho.isp.saktrack.controller.interceptor.OrgAdminOrgAccessCheckInterceptor;
import gov.idaho.isp.saktrack.controller.interceptor.OrgUserPasskeyCheckInterceptor;
import gov.idaho.isp.saktrack.controller.interceptor.OrganizationInterceptor;
import gov.idaho.isp.saktrack.controller.interceptor.RequestAttributeInterceptor;
import gov.idaho.isp.saktrack.controller.interceptor.UserInterceptor;
import gov.idaho.isp.saktrack.mailer.Mailer;
import gov.idaho.isp.saktrack.util.exception.EmailExceptionHandler;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.web.PageableHandlerMethodArgumentResolver;
import org.springframework.web.method.support.HandlerMethodArgumentResolver;
import org.springframework.web.servlet.config.annotation.InterceptorRegistry;
import org.springframework.web.servlet.config.annotation.ResourceHandlerRegistry;
import org.springframework.web.servlet.config.annotation.ViewControllerRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;
import org.springframework.web.servlet.resource.ResourceUrlEncodingFilter;
import org.springframework.web.servlet.resource.VersionResourceResolver;

import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;

@Configuration
public class WebConfig implements WebMvcConfigurer {
  private final UserInterceptor userInterceptor;
  private final DateFormatInterceptor dateFormatInterceptor;
  private final OrgAdminOrgAccessCheckInterceptor orgAdminOrgAccessCheckInterceptor;
  private final OrgUserPasskeyCheckInterceptor orgUserPasskeyCheckInterceptor;
  private final OrganizationInterceptor organizationInterceptor;
  private final FlashForwardInterceptor flashForwardInterceptor;
  private final String activeProfile;
  private final Application.PasswordProperties passwordProperties;

  public WebConfig(UserInterceptor userInterceptor, DateFormatInterceptor dateFormatInterceptor, OrgAdminOrgAccessCheckInterceptor orgAdminOrgAccessCheckInterceptor, OrgUserPasskeyCheckInterceptor orgUserPasskeyCheckInterceptor, OrganizationInterceptor organizationInterceptor, FlashForwardInterceptor flashForwardInterceptor, @Value("${spring.profiles.active}") String activeProfile, Application.PasswordProperties passwordProperties) {
    this.userInterceptor = userInterceptor;
    this.dateFormatInterceptor = dateFormatInterceptor;
    this.orgAdminOrgAccessCheckInterceptor = orgAdminOrgAccessCheckInterceptor;
    this.orgUserPasskeyCheckInterceptor = orgUserPasskeyCheckInterceptor;
    this.organizationInterceptor = organizationInterceptor;
    this.flashForwardInterceptor = flashForwardInterceptor;
    this.activeProfile = activeProfile;
    this.passwordProperties = passwordProperties;
  }

  @Override
  public void addViewControllers(ViewControllerRegistry registry) {
    registry.addViewController("/login").setViewName("public/login");
  }

  @Override
  public void addResourceHandlers(ResourceHandlerRegistry registry) {
    registry
      .addResourceHandler("/assets/**")
      .addResourceLocations("/assets/")
      .setCachePeriod((int) TimeUnit.SECONDS.convert(365L, TimeUnit.DAYS))
      .resourceChain(true)
      .addResolver(new VersionResourceResolver().addContentVersionStrategy("/**"));
  }

  @Override
  public void addInterceptors(InterceptorRegistry registry) {
    registry.addInterceptor(flashForwardInterceptor);
    registry.addInterceptor(userInterceptor);
    registry.addInterceptor(organizationInterceptor);
    registry.addInterceptor(dateFormatInterceptor);
    registry.addInterceptor(new RequestAttributeInterceptor(Map.of("activeProfile", activeProfile, "passwordProperties", passwordProperties)));
    registry.addInterceptor(orgAdminOrgAccessCheckInterceptor).addPathPatterns("/organization/**");
    registry.addInterceptor(orgUserPasskeyCheckInterceptor).excludePathPatterns("/passkey");
  }

  @Override
  public void addArgumentResolvers(List<HandlerMethodArgumentResolver> argumentResolvers) {
    argumentResolvers.add(new PageableHandlerMethodArgumentResolver());
  }

  @Bean
  public ResourceUrlEncodingFilter resourceUrlEncodingFilter() {
    return new ResourceUrlEncodingFilter();
  }

  @Bean
  public EmailExceptionHandler getEmailExceptionHandler(Mailer mailer, @Value("${errors.email.from}") String emailFrom, @Value("${errors.email.to}") String emailTo, @Value("${errors.email.subject}") String emailSubject) {
    EmailExceptionHandler emailExceptionHandler = new EmailExceptionHandler();
    emailExceptionHandler.setErrorView("error");
    emailExceptionHandler.setMailer(mailer);
    emailExceptionHandler.setErrorEmailFrom(emailFrom);
    emailExceptionHandler.setErrorEmailTo(emailTo);
    emailExceptionHandler.setErrorEmailSubject(emailSubject);
    return emailExceptionHandler;
  }
}