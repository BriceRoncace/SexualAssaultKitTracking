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

package gov.idaho.isp.saktrack.controller.organization;

import gov.idaho.isp.saktrack.controller.BaseController;
import gov.idaho.isp.saktrack.domain.jurisdiction.JurisdictionRepository;
import gov.idaho.isp.saktrack.domain.organization.Organization;
import gov.idaho.isp.saktrack.domain.organization.OrganizationRepository;
import gov.idaho.isp.saktrack.domain.organization.OrganizationType;
import gov.idaho.isp.saktrack.domain.user.User;
import gov.idaho.isp.saktrack.domain.user.organization.AbstractOrganizationUser;
import gov.idaho.isp.saktrack.domain.user.organization.OrganizationUserRepository;
import gov.idaho.isp.saktrack.util.RoutingUtil;
import java.util.Arrays;
import java.util.Optional;
import javax.validation.Valid;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestAttribute;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

@Controller
public class SaveOrganizationController extends BaseController {
  private final OrganizationRepository organizationRepository;
  private final OrganizationUserRepository organizationUserRepository;
  private final JurisdictionRepository jurisdictionRepository;

  public SaveOrganizationController(OrganizationRepository organizationRepository, OrganizationUserRepository organizationUserRepository, JurisdictionRepository jurisdictionRepository) {
    this.organizationRepository = organizationRepository;
    this.organizationUserRepository = organizationUserRepository;
    this.jurisdictionRepository = jurisdictionRepository;
  }

  @ModelAttribute
  public Organization prepareOrganization(@RequestParam Optional<Long> orgId, @RequestParam Optional<Long> jurisdictionId) {
    Organization org = new Organization();

    if (orgId.isPresent()) {
      org = organizationRepository.findById(orgId.get()).orElse(null);
    }

    if (jurisdictionId.isPresent()) {
      org.setJurisdiction(jurisdictionRepository.findById(jurisdictionId.get()).orElse(null));
    }

    return org;
  }

  @PostMapping("/organization/save")
  public String saveOrganization(@Valid Organization organization, BindingResult br, Long jurisdictionId, Model model, RedirectAttributes ra, @RequestAttribute User user) {
    if (br.hasErrors()) {
      model.addAttribute("orgTypes", Arrays.asList(OrganizationType.values()));
      model.addAttribute("organization", organization);
      model.addAttribute("errors", getErrors(br));
      return RoutingUtil.getRoute(user) + "/edit-organization";
    }

    organizationRepository.save(organization);
    updateCurrentUserPasskey(user, organization);
    ra.addFlashAttribute("messages", getText("var.save", organization.getName()));
    return "redirect:/";
  }

  @PostMapping("/organization/updatePasskey")
  public String updatePasskey(@RequestParam Long orgId, @RequestParam String passkey, Model model, RedirectAttributes ra, @RequestAttribute User user) {
    Organization organization = organizationRepository.findById(orgId).orElse(null);
    organization.setPasskey(passkey);
    organizationRepository.save(organization);
    updateCurrentUserPasskey(user, organization);
    ra.addFlashAttribute("messages", getText("var.save", "Passkey"));
    return "redirect:/";
  }

  private void updateCurrentUserPasskey(User user, Organization organization) {
    if (user instanceof AbstractOrganizationUser) {
      AbstractOrganizationUser orgUser = (AbstractOrganizationUser) user;
      orgUser.setPasskey(organization.getPasskey());
      organizationUserRepository.save(orgUser);
    }
  }
}
