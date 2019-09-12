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
import gov.idaho.isp.saktrack.domain.SexualAssaultKit;
import gov.idaho.isp.saktrack.domain.SexualAssaultKitRepository;
import gov.idaho.isp.saktrack.domain.organization.Organization;
import gov.idaho.isp.saktrack.domain.organization.OrganizationRepository;
import gov.idaho.isp.saktrack.domain.user.User;
import gov.idaho.isp.saktrack.service.AuditService;
import java.util.Optional;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestAttribute;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

@Controller
public class SetCurrentAssignmentController extends BaseController {
  private final SexualAssaultKitRepository sexualAssaultKitRepository;
  private final OrganizationRepository organizationRepository;
  private final AuditService auditService;

  public SetCurrentAssignmentController(SexualAssaultKitRepository sexualAssaultKitRepository, OrganizationRepository organizationRepository, AuditService auditService) {
    this.sexualAssaultKitRepository = sexualAssaultKitRepository;
    this.organizationRepository = organizationRepository;
    this.auditService = auditService;
  }

  @PostMapping("/admin/setAssignment")
  public String setAssignment(@RequestParam Long id, @RequestParam Optional<Long> currentAssignmentId, @RequestParam String reason, @RequestAttribute User user, Model model, RedirectAttributes ra) {
    SexualAssaultKit kit = sexualAssaultKitRepository.findById(id).orElse(null);
    kit.setCurrentAssignment(getCurrentAssignmentOrNull(currentAssignmentId));
    auditService.auditKit(kit, reason, user);
    sexualAssaultKitRepository.save(kit);
    ra.addFlashAttribute("messages", getText("kit", kit.getSerialNumber(), " saved"));
    return "redirect:/admin/manageEvents?kitId=" + id;
  }

  private Organization getCurrentAssignmentOrNull(Optional<Long> currentAssignmentId) {
    return currentAssignmentId.isPresent() ? organizationRepository.findById(currentAssignmentId.get()).orElse(null) : null;
  }
}
