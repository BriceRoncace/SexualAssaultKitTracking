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

import gov.idaho.isp.saktrack.domain.SexualAssaultKit;
import gov.idaho.isp.saktrack.controller.BaseController;
import gov.idaho.isp.saktrack.domain.SexualAssaultKitRepository;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

@Controller
public class SaveLegalDetailsController extends BaseController {
  private final SexualAssaultKitRepository sexualAssaultKitRepository;

  public SaveLegalDetailsController(SexualAssaultKitRepository sexualAssaultKitRepository) {
    this.sexualAssaultKitRepository = sexualAssaultKitRepository;
  }

  @ModelAttribute
  public SexualAssaultKit prepareKit(@RequestParam Long id) {
    return sexualAssaultKitRepository.findById(id).orElse(null);
  }

  @PostMapping("/legal/saveDetails")
  public String saveDetails(SexualAssaultKit kit, Model model, RedirectAttributes ra) {
    sexualAssaultKitRepository.save(kit);
    ra.addFlashAttribute("messages", getText("kit", kit.getSerialNumber(), " legal details saved"));
    return "redirect:/legal/dashboard";
  }
}
