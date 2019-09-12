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
import gov.idaho.isp.saktrack.domain.SexualAssaultKitRepository;
import gov.idaho.isp.saktrack.domain.organization.Organization;
import gov.idaho.isp.saktrack.domain.organization.OrganizationRepository;
import gov.idaho.isp.saktrack.domain.search.SexualAssaultKitSearchCriteria;
import gov.idaho.isp.saktrack.domain.search.SexualAssaultKitSpec;
import gov.idaho.isp.saktrack.domain.user.User;
import gov.idaho.isp.saktrack.domain.user.organization.AbstractOrganizationUser;
import gov.idaho.isp.saktrack.domain.user.organization.OrganizationUserRepository;
import java.util.List;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestAttribute;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

@Controller
public class RemoveOrganizationContoller extends BaseController {
  private final OrganizationRepository organizationRepository;
  private final OrganizationUserRepository organizationUserRepository;
  private final SexualAssaultKitRepository sexualAssaultKitRepository;

  public RemoveOrganizationContoller(OrganizationRepository organizationRepository, OrganizationUserRepository organizationUserRepository, SexualAssaultKitRepository sexualAssaultKitRepository) {
    this.organizationRepository = organizationRepository;
    this.organizationUserRepository = organizationUserRepository;
    this.sexualAssaultKitRepository = sexualAssaultKitRepository;
  }

  @PostMapping("/organization/{orgId}/remove")
  public String removeOrganization(@PathVariable Long orgId, RedirectAttributes ra, @RequestAttribute User user) {
    Organization org = organizationRepository.findById(orgId).orElse(null);
    if (isOrganizationAssociatedToAnyKits(org)) {
      ra.addFlashAttribute("errors", String.format("%s cannot be removed because it is associated to sexual assault kits.", org.getName()));
      return "redirect:/";
    }

    removeAllUsers(org);
    removeOrganization(org);
    ra.addFlashAttribute("messages", getText("var.remove", org.getName()));
    return "redirect:/";
  }

  private boolean isOrganizationAssociatedToAnyKits(Organization org) {
    return isOrganizationInAnyKitChainOfCustody(org) || hasOrganizationReviewedAnyKit(org);
  }

  private boolean isOrganizationInAnyKitChainOfCustody(Organization org) {
    SexualAssaultKitSearchCriteria criteria = new SexualAssaultKitSearchCriteria();
    criteria.setEventOrganization(org.getId());
    return sexualAssaultKitRepository.count(new SexualAssaultKitSpec(criteria)) > 0;
  }

  private boolean hasOrganizationReviewedAnyKit(Organization org) {
    SexualAssaultKitSearchCriteria criteria = new SexualAssaultKitSearchCriteria();
    criteria.setReviewingProsecutorOrganization(org.getId());
    return sexualAssaultKitRepository.count(new SexualAssaultKitSpec(criteria)) > 0;
  }

  private void removeAllUsers(Organization org) {
    List<AbstractOrganizationUser> users = organizationUserRepository.findByOrganizationIdOrderByDisplayNameAsc(org.getId());
    organizationUserRepository.deleteAll(users);
  }

  private void removeOrganization(Organization org) {
    organizationRepository.delete(org);
  }
}
