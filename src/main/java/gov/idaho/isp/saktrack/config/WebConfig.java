package gov.idaho.isp.saktrack.config;

import gov.idaho.isp.mail.Mailer;
import gov.idaho.isp.saktrack.controller.advice.EmailExceptionHandler;
import gov.idaho.isp.saktrack.controller.interceptor.DateFormatInterceptor;
import gov.idaho.isp.saktrack.controller.interceptor.ModelToFlashAttributeInterceptor;
import gov.idaho.isp.saktrack.controller.interceptor.OrgAdminOrgAccessCheckInterceptor;
import gov.idaho.isp.saktrack.controller.interceptor.OrgUserPasskeyCheckInterceptor;
import gov.idaho.isp.saktrack.controller.interceptor.OrganizationInterceptor;
import gov.idaho.isp.saktrack.controller.interceptor.UserInterceptor;
import gov.idaho.isp.saktrack.propertyeditor.LocalDatePropertyEditor;
import gov.idaho.isp.saktrack.propertyeditor.LocalDateTimePropertyEditor;
import java.util.List;
import java.util.concurrent.TimeUnit;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.MessageSource;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.PropertySource;
import org.springframework.context.support.PropertySourcesPlaceholderConfigurer;
import org.springframework.context.support.ReloadableResourceBundleMessageSource;
import org.springframework.data.web.PageableHandlerMethodArgumentResolver;
import org.springframework.web.method.support.HandlerMethodArgumentResolver;
import org.springframework.web.servlet.config.annotation.EnableWebMvc;
import org.springframework.web.servlet.config.annotation.InterceptorRegistry;
import org.springframework.web.servlet.config.annotation.ResourceHandlerRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurerAdapter;
import org.springframework.web.servlet.resource.VersionResourceResolver;
import org.springframework.web.servlet.view.InternalResourceViewResolver;

@Configuration
@EnableWebMvc
@ComponentScan(basePackages = {"gov.idaho.isp.saktrack.controller", "gov.idaho.isp.saktrack.validation"})
@PropertySource("classpath:spring.properties")
public class WebConfig extends WebMvcConfigurerAdapter {
  @Autowired
  private UserInterceptor userInterceptor;
  @Autowired
  private DateFormatInterceptor dateFormatInterceptor;
  @Autowired
  private OrgAdminOrgAccessCheckInterceptor orgAdminOrgAccessCheckInterceptor;
  @Autowired
  private OrgUserPasskeyCheckInterceptor orgUserPasskeyCheckInterceptor;
  @Autowired
  private OrganizationInterceptor organizationInterceptor;
  @Autowired
  private ModelToFlashAttributeInterceptor modelToFlashAttributeInterceptor;

  @Bean
  public static PropertySourcesPlaceholderConfigurer propertySourcesPlaceholderConfigurer() {
    return new PropertySourcesPlaceholderConfigurer();
  }

  @Bean
  public InternalResourceViewResolver getInternalResourceViewResolver() {
    InternalResourceViewResolver resolver = new InternalResourceViewResolver();
    resolver.setPrefix("/WEB-INF/views/");
    resolver.setSuffix(".jsp");
    return resolver;
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
    registry.addInterceptor(modelToFlashAttributeInterceptor);
    registry.addInterceptor(userInterceptor);
    registry.addInterceptor(organizationInterceptor);
    registry.addInterceptor(dateFormatInterceptor);
    registry.addInterceptor(orgAdminOrgAccessCheckInterceptor).addPathPatterns("/organization/**");
    registry.addInterceptor(orgUserPasskeyCheckInterceptor).excludePathPatterns("/passkey");
  }

  @Override
  public void addArgumentResolvers(List<HandlerMethodArgumentResolver> argumentResolvers) {
    argumentResolvers.add(new PageableHandlerMethodArgumentResolver());
  }

  @Bean
  public LocalDatePropertyEditor getLocalDatePropertyEditor(@Value("${date.format}") String format) {
    return new LocalDatePropertyEditor(format);
  }

  @Bean
  public LocalDateTimePropertyEditor getLocalDateTimePropertyEditor(@Value("${date.time.format}") String format) {
    return new LocalDateTimePropertyEditor(format);
  }

  @Bean
  public MessageSource messageSource() {
    ReloadableResourceBundleMessageSource messageSource = new ReloadableResourceBundleMessageSource();
    messageSource.setBasename("classpath:messages");
    messageSource.setDefaultEncoding("UTF-8");
    return messageSource;
  }

  @Bean
  public EmailExceptionHandler getEmailExceptionHandler(Mailer mailer, @Value("${errors.email.from}") String emailFrom, @Value("${errors.email.to}") String emailTo, @Value("${errors.email.subject}") String emailSubject) {
    mailer.setSendAsynchronously(true);
    EmailExceptionHandler emailExceptionHandler = new EmailExceptionHandler();
    emailExceptionHandler.setErrorView("public/error");
    emailExceptionHandler.setMailer(mailer);
    emailExceptionHandler.setErrorEmailFrom(emailFrom);
    emailExceptionHandler.setErrorEmailTo(emailTo);
    emailExceptionHandler.setErrorEmailSubject(emailSubject);
    return emailExceptionHandler;
  }
}
