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

import gov.idaho.isp.saktrack.domain.user.AbstractUserRepository;
import gov.idaho.isp.saktrack.domain.user.AbstractUserSpec;
import gov.idaho.isp.saktrack.service.csv.CsvExportService;
import org.springframework.http.HttpEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;

@Controller
public class ExportUsersAsCsvController {
  private final AbstractUserRepository abstractUserRepository;
  private final CsvExportService csvExportService;

  public ExportUsersAsCsvController(AbstractUserRepository abstractUserRepository, CsvExportService csvExportService) {
    this.abstractUserRepository = abstractUserRepository;
    this.csvExportService = csvExportService;
  }

  @GetMapping("/users/download")
  public HttpEntity<byte[]> downloadUsers(AbstractUserSpec spec) {
    return csvExportService.exportUserList(abstractUserRepository.findAll(spec)).toHttpEntity();
  }
}
