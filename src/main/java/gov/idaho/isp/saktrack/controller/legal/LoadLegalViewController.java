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

package gov.idaho.isp.saktrack.controller.legal;

import gov.idaho.isp.saktrack.domain.LawEnforcementDetails.NonSubmissionReason;
import gov.idaho.isp.saktrack.domain.SexualAssaultKit;
import gov.idaho.isp.saktrack.domain.SexualAssaultKitRepository;
import gov.idaho.isp.saktrack.domain.user.User;
import gov.idaho.isp.saktrack.util.RoutingUtil;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestAttribute;
import org.springframework.web.bind.annotation.RequestParam;

@Controller
public class LoadLegalViewController {
  private final SexualAssaultKitRepository sexualAssaultKitRepository;

  public LoadLegalViewController(SexualAssaultKitRepository sexualAssaultKitRepository) {
    this.sexualAssaultKitRepository = sexualAssaultKitRepository;
  }

  @GetMapping("/legal/view")
  public String viewOrEditKit(@RequestParam Long id, @RequestAttribute User user, Model model) {
    SexualAssaultKit kit = sexualAssaultKitRepository.findById(id).orElse(null);
    model.addAttribute("kit", kit);
    model.addAttribute("nonSubmissionReasons", NonSubmissionReason.values());
    return RoutingUtil.getLoadKitView(kit, user);
  }
}
