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

import gov.idaho.isp.saktrack.domain.user.AdminUserRepository;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;

@Controller
public class ManageAdminUserController {
  private final AdminUserRepository adminUserRepository;

  public ManageAdminUserController(AdminUserRepository adminUserRepository) {
    this.adminUserRepository = adminUserRepository;
  }

  @GetMapping("/adminUser")
  public String newAdminUser(Model model) {
    model.addAttribute("adminUsers", adminUserRepository.findAll(Sort.by("displayName")));
    return "admin/manage-admin";
  }
}
