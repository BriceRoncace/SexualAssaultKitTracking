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

import gov.idaho.isp.saktrack.controller.BaseController;
import gov.idaho.isp.saktrack.domain.user.AdminUserRepository;
import gov.idaho.isp.saktrack.domain.user.User;
import gov.idaho.isp.saktrack.domain.user.dto.AdminUserForm;
import gov.idaho.isp.saktrack.service.UserFormFactory;
import java.util.Optional;
import javax.validation.Valid;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestAttribute;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

@Controller
public class SaveAdminUserController extends BaseController {
  private final AdminUserRepository adminUserRepository;
  private final UserFormFactory userFormFactory;
  private final PasswordEncoder passwordEncoder;

  public SaveAdminUserController(AdminUserRepository adminUserRepository, UserFormFactory userFormFactory, PasswordEncoder passwordEncoder) {
    this.adminUserRepository = adminUserRepository;
    this.userFormFactory = userFormFactory;
    this.passwordEncoder = passwordEncoder;
  }

  @ModelAttribute
  public AdminUserForm prepareAdminUserForm(@RequestParam Optional<Long> userId, @RequestParam("passwordPair.oldPassword") Optional<String> oldPassword) {
    return userFormFactory.getAdminUserForm(userId, oldPassword.isPresent());
  }

  @GetMapping("/adminUser/save")
  public String toAdminUserForm(@Valid AdminUserForm adminUserForm, BindingResult br, Model model, RedirectAttributes ra, @RequestAttribute User user) {
    model.addAttribute("adminUser", adminUserForm.getAdminUser());
    return "/admin/save-admin-user";
  }

  @PostMapping("/adminUser/save")
  public String saveAdminUser(@Valid AdminUserForm adminUserForm, BindingResult br, Model model, RedirectAttributes ra, @RequestAttribute User user) {
    if (br.hasErrors()) {
      model.addAttribute("errors", getErrors(br));
      model.addAttribute("adminUser", adminUserForm.getAdminUser());
      return "/admin/save-admin-user";
    }

    adminUserForm.encodePasswordIfNecessary(passwordEncoder);
    adminUserRepository.save(adminUserForm.getAdminUser());
    ra.addFlashAttribute("messages", getText("var.save", adminUserForm.getAdminUser().getDisplayName()));
    return "redirect:/adminUser";
  }
}
