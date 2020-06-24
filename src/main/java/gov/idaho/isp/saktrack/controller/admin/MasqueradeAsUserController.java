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

package gov.idaho.isp.saktrack.controller.admin;

import gov.idaho.isp.saktrack.domain.user.organization.AbstractOrganizationUser;
import gov.idaho.isp.saktrack.domain.user.organization.OrganizationUserRepository;
import gov.idaho.isp.saktrack.security.CustomDatabaseUserDetailsService;
import java.util.Optional;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

@Controller
public class MasqueradeAsUserController {
  private final OrganizationUserRepository organizationUserRepository;

  public MasqueradeAsUserController(OrganizationUserRepository organizationUserRepository) {
    this.organizationUserRepository = organizationUserRepository;
  }

  @GetMapping("/masqueradeAs")
  public String newAdminUser(@RequestParam Long userId, RedirectAttributes ra) {
    Optional<AbstractOrganizationUser> user = organizationUserRepository.findById(userId);
    user.ifPresent(u -> {
      SecurityContextHolder.getContext().setAuthentication(toAuthentication(u));
      ra.addFlashAttribute("messages", "You are now masquerading as " + u.getDisplayName());
    });
    return "redirect:/";
  }

  private Authentication toAuthentication(AbstractOrganizationUser user) {
    UserDetails userDetails = CustomDatabaseUserDetailsService.loadOrganizationUser(user);
    UsernamePasswordAuthenticationToken auth = new UsernamePasswordAuthenticationToken(userDetails, userDetails.getPassword(), userDetails.getAuthorities());
    auth.setDetails(userDetails);
    return auth;
  }
}
