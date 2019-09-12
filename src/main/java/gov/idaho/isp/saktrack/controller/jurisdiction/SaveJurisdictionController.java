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

package gov.idaho.isp.saktrack.controller.jurisdiction;

import gov.idaho.isp.saktrack.controller.BaseController;
import gov.idaho.isp.saktrack.domain.jurisdiction.Jurisdiction;
import gov.idaho.isp.saktrack.domain.jurisdiction.JurisdictionRepository;
import gov.idaho.isp.saktrack.domain.user.User;
import java.util.Arrays;
import java.util.Optional;
import javax.validation.Valid;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestAttribute;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

@Controller
public class SaveJurisdictionController extends BaseController {
  private final JurisdictionRepository jurisdictionRepository;

  public SaveJurisdictionController(JurisdictionRepository jurisdictionRepository) {
    this.jurisdictionRepository = jurisdictionRepository;
  }

  @ModelAttribute
  public Jurisdiction loadJurisdiction(@RequestParam Optional<Long> jurisdictionId) {
    return jurisdictionId.isPresent() ? jurisdictionRepository.findById(jurisdictionId.get()).orElse(null) : new Jurisdiction();
  }

  @PostMapping("/jurisdiction/save")
  public String saveJurisdiction(@Valid Jurisdiction jurisdiction, BindingResult br, Model model, RedirectAttributes ra, @RequestAttribute User user) {
    if (br.hasErrors()) {
      model.addAttribute("jurisdictions", jurisdictionRepository.findAll(Sort.by("name")));
      model.addAttribute("jurisdictionTypes", Arrays.asList(Jurisdiction.Type.values()));
      model.addAttribute("errors", getErrors(br));
      return "admin/jurisdictions";
    }

    jurisdictionRepository.save(jurisdiction);
    ra.addFlashAttribute("messages", getText("var.save", jurisdiction.getName()));
    return "redirect:/jurisdictions/list";
  }

}
