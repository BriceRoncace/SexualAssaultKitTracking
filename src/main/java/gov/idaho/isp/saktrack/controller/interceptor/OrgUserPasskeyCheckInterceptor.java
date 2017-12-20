package gov.idaho.isp.saktrack.controller.interceptor;

import gov.idaho.isp.saktrack.user.organization.OrganizationUser;
import gov.idaho.isp.saktrack.user.persistence.OrganizationUserRepository;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.stereotype.Component;
import org.springframework.web.servlet.handler.HandlerInterceptorAdapter;

@Component
public class OrgUserPasskeyCheckInterceptor extends HandlerInterceptorAdapter {
  private final OrganizationUserRepository organizationUserRepository;

  public OrgUserPasskeyCheckInterceptor(OrganizationUserRepository organizationUserRepository) {
    this.organizationUserRepository = organizationUserRepository;
  }

  @Override
  public boolean preHandle(HttpServletRequest request, HttpServletResponse response, Object handler) throws Exception {
    if (SecurityContextHolder.getContext() != null && SecurityContextHolder.getContext().getAuthentication() != null) {
      final Object userDetails = SecurityContextHolder.getContext().getAuthentication().getPrincipal();

      if (userDetails != null && userDetails instanceof UserDetails) {
        UserDetails details = (UserDetails) userDetails;
        OrganizationUser user = organizationUserRepository.findByUsernameIgnoreCase(details.getUsername());
        if (user != null && (user.getPasskey() == null || !user.getPasskey().equals(user.getOrganization().getPasskey()))) {
          response.sendRedirect(request.getContextPath() + "/passkey");
          return false;
        }
      }
    }
    return true;
  }
}
