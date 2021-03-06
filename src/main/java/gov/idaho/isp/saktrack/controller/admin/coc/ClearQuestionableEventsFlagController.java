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

package gov.idaho.isp.saktrack.controller.admin.coc;

import gov.idaho.isp.saktrack.domain.SexualAssaultKit;
import gov.idaho.isp.saktrack.domain.SexualAssaultKitRepository;
import gov.idaho.isp.saktrack.domain.user.User;
import gov.idaho.isp.saktrack.service.AuditService;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestAttribute;
import org.springframework.web.bind.annotation.RequestParam;

@Controller
public class ClearQuestionableEventsFlagController {
  private final SexualAssaultKitRepository sexualAssaultKitRepository;
  private final AuditService auditService;

  public ClearQuestionableEventsFlagController(SexualAssaultKitRepository sexualAssaultKitRepository, AuditService auditService) {
    this.sexualAssaultKitRepository = sexualAssaultKitRepository;
    this.auditService = auditService;
  }

  @PostMapping("/admin/clearQuestionable")
  public String clearQuestionableEventsFlag(@RequestParam Long kitId, @RequestParam String reason, @RequestAttribute User user) {
    SexualAssaultKit kit = sexualAssaultKitRepository.findById(kitId).orElse(null);
    if (kit != null) {
      kit.setQuestionableEvents(false);
    }
    auditService.auditKit(kit, reason, user);
    sexualAssaultKitRepository.save(kit);
    return "redirect:/admin/manageEvents?kitId=" + kitId;
  }
}
