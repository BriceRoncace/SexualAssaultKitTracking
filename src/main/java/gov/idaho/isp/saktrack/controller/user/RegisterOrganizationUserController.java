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
import gov.idaho.isp.saktrack.domain.organization.OrganizationRepository;
import gov.idaho.isp.saktrack.domain.user.dto.OrgUserForm;
import gov.idaho.isp.saktrack.domain.user.organization.OrganizationUserRepository;
import gov.idaho.isp.saktrack.service.email.EmailService;
import gov.idaho.isp.saktrack.service.UserFormFactory;
import java.util.Optional;
import jakarta.validation.Valid;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

@Controller
public class RegisterOrganizationUserController extends BaseController {
  private final OrganizationRepository organizationRepository;
  private final OrganizationUserRepository organizationUserRepository;
  private final UserFormFactory userFormFactory;
  private final PasswordEncoder passwordEncoder;
  private final EmailService emailService;

  public RegisterOrganizationUserController(OrganizationRepository organizationRepository, OrganizationUserRepository organizationUserRepository, UserFormFactory userFormFactory, PasswordEncoder passwordEncoder, EmailService emailService) {
    this.organizationRepository = organizationRepository;
    this.organizationUserRepository = organizationUserRepository;
    this.userFormFactory = userFormFactory;
    this.passwordEncoder = passwordEncoder;
    this.emailService = emailService;
  }

  @ModelAttribute
  public OrgUserForm prepareOrganizationUser(@RequestParam Optional<Long> orgId) {
    return userFormFactory.getOrgUserForm(orgId);
  }

  @PostMapping("/register")
  public String saveRegisterOrganizationUser(@Valid OrgUserForm orgUserForm, BindingResult br, Model model, RedirectAttributes ra) {
    if (br.hasErrors()) {
      model.addAttribute("errors", getErrors(br));
      model.addAttribute("organizations", organizationRepository.findByEnabledOrderByNameAsc(true));
      model.addAttribute("orgUser", orgUserForm.getOrgUser());
      return "public/user-registration";
    }

    orgUserForm.encodePasswordIfNecessary(passwordEncoder);
    organizationUserRepository.save(orgUserForm.getOrgUser());
    emailService.sendUserRegisteredEmail(orgUserForm.getOrgUser());
    ra.addFlashAttribute("messages", getText("register.success", orgUserForm.getOrgUser().getDisplayName()));
    return "redirect:/login";
  }
}
