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

import gov.idaho.isp.saktrack.domain.organization.OrganizationRepository;
import gov.idaho.isp.saktrack.domain.user.User;
import gov.idaho.isp.saktrack.domain.user.organization.OrganizationUser;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;
import org.springframework.web.servlet.HandlerInterceptor;

import java.util.Arrays;
import java.util.List;

@Component
public class OrganizationInterceptor implements HandlerInterceptor {
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
