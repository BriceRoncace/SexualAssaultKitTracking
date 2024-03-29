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

package gov.idaho.isp.saktrack.controller.interceptor;

import gov.idaho.isp.saktrack.domain.user.organization.OrganizationUser;
import gov.idaho.isp.saktrack.domain.user.organization.OrganizationUserRepository;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.stereotype.Component;
import org.springframework.web.servlet.HandlerInterceptor;

import java.util.Arrays;
import java.util.List;

@Component
public class OrgAdminOrgAccessCheckInterceptor implements HandlerInterceptor {
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
