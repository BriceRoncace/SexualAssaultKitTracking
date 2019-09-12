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

package gov.idaho.isp.saktrack.controller.user;

import gov.idaho.isp.saktrack.controller.BaseController;
import gov.idaho.isp.saktrack.domain.user.User;
import gov.idaho.isp.saktrack.domain.user.organization.AbstractOrganizationUser;
import gov.idaho.isp.saktrack.domain.user.organization.OrganizationUserRepository;
import gov.idaho.isp.saktrack.util.UserUtils;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestAttribute;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

@Controller
public class RemoveOrganizationUserController extends BaseController {
  private final OrganizationUserRepository organizationUserRepository;

  public RemoveOrganizationUserController(OrganizationUserRepository organizationUserRepository) {
    this.organizationUserRepository = organizationUserRepository;
  }

  @PostMapping("/organization/{orgId}/user/remove")
  public String removeUser(@RequestParam Long id, RedirectAttributes ra, @RequestAttribute User user) {
    AbstractOrganizationUser orgUser = organizationUserRepository.findById(id).orElse(null);
    organizationUserRepository.delete(orgUser);
    ra.addFlashAttribute("messages", getText("var.remove", orgUser.getDisplayName()));
    return UserUtils.isSameUserEntity(user, orgUser) ? "redirect:/logout" : "redirect:/organization/" + orgUser.getOrganization().getId();
  }
}
