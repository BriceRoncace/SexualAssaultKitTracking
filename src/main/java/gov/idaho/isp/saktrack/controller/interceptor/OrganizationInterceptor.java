package gov.idaho.isp.saktrack.controller.interceptor;

import gov.idaho.isp.saktrack.domain.organization.OrganizationRepository;
import gov.idaho.isp.saktrack.domain.user.User;
import gov.idaho.isp.saktrack.domain.user.organization.OrganizationUser;
import java.util.Arrays;
import java.util.List;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;
import org.springframework.web.servlet.handler.HandlerInterceptorAdapter;

@Component
public class OrganizationInterceptor extends HandlerInterceptorAdapter {
  private final OrganizationRepository organizationRepository;

  public OrganizationInterceptor(OrganizationRepository organizationRepository) {
    this.organizationRepository = organizationRepository;
  }

  @Override
  public boolean preHandle(HttpServletRequest req, HttpServletResponse res, Object handler) throws Exception {
    if (SecurityContextHolder.getContext() != null && SecurityContextHolder.getContext().getAuthentication() != null) {
      User user = (User) req.getAttribute("user");
      if (user != null && user instanceof OrganizationUser) {
        OrganizationUser orgUser = (OrganizationUser) user;
        req.setAttribute("organization", orgUser.getOrganization());
        return true;
      }
      Long orgId = findOrgId(req);
      if (orgId != null) {
        req.setAttribute("organization", organizationRepository.findById(orgId).orElse(null));
        return true;
      }
    }
    return true;
  }

  private Long findOrgId(HttpServletRequest request) {
    String id = request.getParameter("orgId") != null ? request.getParameter("orgId") : getOrgIdFromUrl(request.getRequestURL().toString());
    try {
      return Long.parseLong(id);
    }
    catch(Exception e) {
      return null;
    }
  }

  private String getOrgIdFromUrl(String url) {
    if (url.contains("organization/")) {
      url = url.substring(url.indexOf("organization/"));
      List<String> parts = Arrays.asList(url.split("/"));
      return parts.get(1);
    }
    return null;
  }
}
