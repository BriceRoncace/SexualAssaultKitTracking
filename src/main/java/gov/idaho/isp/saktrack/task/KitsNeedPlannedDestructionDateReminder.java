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

package gov.idaho.isp.saktrack.task;

import gov.idaho.isp.saktrack.domain.SexualAssaultKit;
import gov.idaho.isp.saktrack.domain.SexualAssaultKitRepository;
import gov.idaho.isp.saktrack.domain.organization.Organization;
import gov.idaho.isp.saktrack.service.email.EmailService;
import java.time.LocalDate;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import org.springframework.stereotype.Service;

@Service(value = "kitsNeedPlannedDestructionDateReminder")
public class KitsNeedPlannedDestructionDateReminder implements Task {
  private final SexualAssaultKitRepository sexualAssaultKitRepository;
  private final EmailService emailService;

  public KitsNeedPlannedDestructionDateReminder(SexualAssaultKitRepository sexualAssaultKitRepository, EmailService emailService) {
    this.sexualAssaultKitRepository = sexualAssaultKitRepository;
    this.emailService = emailService;
  }

  @Override
  public void run() {
    LocalDate collectedAYearAgo = LocalDate.now().minusYears(1);
    List<SexualAssaultKit> kits = sexualAssaultKitRepository.findKitsNeedDestructionDateReminder(collectedAYearAgo);
    Map<Organization, List<SexualAssaultKit>> kitMap = kits.stream().filter(kit -> kit.getMedicalDetails().getRequestingLeAgency() != null).collect(Collectors.groupingBy(kit -> kit.getMedicalDetails().getRequestingLeAgency()));
    kitMap.entrySet().forEach(entry -> emailService.sendPlannedDestructionDateReminder(entry.getValue()));
  }
}
