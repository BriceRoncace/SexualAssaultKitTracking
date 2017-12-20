package gov.idaho.isp.saktrack.controller.interceptor;

import gov.idaho.isp.saktrack.user.organization.AbstractOrganizationUser;
import gov.idaho.isp.saktrack.user.persistence.OrganizationUserRepository;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.stereotype.Component;
import org.springframework.web.servlet.handler.HandlerInterceptorAdapter;

@Component
public class UserInterceptor extends HandlerInterceptorAdapter {
  private final OrganizationUserRepository organizationUserRepository;

  public UserInterceptor(OrganizationUserRepository organizationUserRepository) {
    this.organizationUserRepository = organizationUserRepository;
  }

  @Override
  public boolean preHandle(HttpServletRequest request, HttpServletResponse response, Object handler) throws Exception {
    if (SecurityContextHolder.getContext() != null && SecurityContextHolder.getContext().getAuthentication() != null) {
      Object userDetails = SecurityContextHolder.getContext().getAuthentication().getPrincipal();
      if (userDetails != null && userDetails instanceof UserDetails) {
        if (userDetails instanceof AbstractOrganizationUser) {
          AbstractOrganizationUser u = (AbstractOrganizationUser) userDetails;
          if (!u.hasDependencies()) {
            userDetails = organizationUserRepository.findByUsernameIgnoreCase(u.getUsername());
          }
        }
        request.setAttribute("user", userDetails);
      }
    }
    return true;
  }
}
