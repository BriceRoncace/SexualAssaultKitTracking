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

import gov.idaho.isp.saktrack.domain.jurisdiction.JurisdictionRepository;
import gov.idaho.isp.saktrack.exception.SexualAssaultKitTrackingException;
import gov.idaho.isp.saktrack.service.DataImporter;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

import java.util.function.Function;

@Controller
public class DataImportController {
  private final DataImporter dataImporter;
  private final JurisdictionRepository jurisdictionRepository;

  public DataImportController(DataImporter dataImporter, JurisdictionRepository jurisdictionRepository) {
    this.dataImporter = dataImporter;
    this.jurisdictionRepository = jurisdictionRepository;
  }

  @GetMapping("/dataImport")
  public String dataMigration(Model model) {
    model.addAttribute("jurisdictions", jurisdictionRepository.findAll(Sort.by("name")));
    return "admin/data-import";
  }

  @PostMapping("/orgImport")
  public String orgImport(MultipartFile file, RedirectAttributes ra) {
    return processFile(file, ra, dataImporter::importNewOrganizations);
  }

  @PostMapping("/userImport")
  public String userImport(MultipartFile file, RedirectAttributes ra) {
    return processFile(file, ra, dataImporter::importNewOrganizationUsers);
  }

  private String processFile(MultipartFile file, RedirectAttributes ra, Function<MultipartFile, Integer> function) {
    if (!file.isEmpty()) {
      try {
        Integer records = function.apply(file);
        ra.addFlashAttribute("messages", String.format("%s records added.", records));
      }
      catch (SexualAssaultKitTrackingException ex) {
        ra.addFlashAttribute("errors", ex.getErrors());
      }
    }
    return "redirect:/dataImport";
  }
}
