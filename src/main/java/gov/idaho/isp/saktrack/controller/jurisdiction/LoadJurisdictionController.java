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
import java.util.Arrays;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;

@Controller
public class LoadJurisdictionController extends BaseController {
  private final JurisdictionRepository jurisdictionRepository;

  public LoadJurisdictionController(JurisdictionRepository jurisdictionRepository) {
    this.jurisdictionRepository = jurisdictionRepository;
  }

  @GetMapping("/jurisdiction/{id}")
  public String loadJurisdiction(@PathVariable Long id, Model model) {
    if (id != null) {
      model.addAttribute("jurisdiction", jurisdictionRepository.findById(id).orElse(null));
    }

    model.addAttribute("jurisdictions", jurisdictionRepository.findAll(Sort.by("name")));
    model.addAttribute("jurisdictionTypes", Arrays.asList(Jurisdiction.Type.values()));
    return "admin/jurisdictions";
  }
}
