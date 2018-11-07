package gov.idaho.isp.saktrack.controller.interceptor;

import gov.idaho.isp.saktrack.domain.user.organization.OrganizationUser;
import gov.idaho.isp.saktrack.domain.user.organization.OrganizationUserRepository;
import java.util.Arrays;
import java.util.List;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.stereotype.Component;
import org.springframework.web.servlet.handler.HandlerInterceptorAdapter;

@Component
public class OrgAdminOrgAccessCheckInterceptor extends HandlerInterceptorAdapter {
  private final OrganizationUserRepository organizationUserRepository;

  public OrgAdminOrgAccessCheckInterceptor(OrganizationUserRepository organizationUserRepository) {
    this.organizationUserRepository = organizationUserRepository;
  }

  @Override
  public boolean preHandle(HttpServletRequest req, HttpServletResponse res, Object handler) throws Exception {
    if (SecurityContextHolder.getContext() != null && SecurityContextHolder.getContext().getAuthentication() != null) {
      final Object userDetails = SecurityContextHolder.getContext().getAuthentication().getPrincipal();
      final Long orgId = getOrgIdFromUrl(req.getRequestURL().toString());

      if (userDetails != null && userDetails instanceof UserDetails && orgId != null) {
        UserDetails details = (UserDetails) userDetails;
        OrganizationUser user = organizationUserRepository.findByUsernameIgnoreCase(details.getUsername());

        if (user != null && !orgId.equals(user.getOrganization().getId())) {
          res.sendRedirect(req.getContextPath() + "/" + user.getType().getLabel() + "/dashboard?errors=Access+denied.");
          return false;
        }

      }
    }
    return true;
  }

  private Long getOrgIdFromUrl(String url) {
    url = url.substring(url.indexOf("organization/"));
    List<String> parts = Arrays.asList(url.split("/"));
    try {
      return Long.parseLong(parts.get(1));
    }
    catch (Exception e) {
      return null;
    }
  }
}
