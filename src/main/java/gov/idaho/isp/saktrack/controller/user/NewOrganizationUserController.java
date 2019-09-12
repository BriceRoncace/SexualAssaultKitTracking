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

import gov.idaho.isp.saktrack.domain.organization.OrganizationRepository;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;

@Controller
public class NewOrganizationUserController {
  private final OrganizationRepository organizationRepository;

  public NewOrganizationUserController(OrganizationRepository organizationRepository) {
    this.organizationRepository = organizationRepository;
  }

  @GetMapping("/organization/{orgId}/user/new")
  public String newOrganizationUser(@PathVariable Long orgId, Model model) {
    model.addAttribute("organization", organizationRepository.findById(orgId).orElse(null));
    return "org-users/save-user";
  }
}
