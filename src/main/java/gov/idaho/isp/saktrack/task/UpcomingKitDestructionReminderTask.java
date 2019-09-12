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
import gov.idaho.isp.saktrack.service.email.EmailService;
import java.time.LocalDate;
import java.util.List;
import org.springframework.stereotype.Service;

@Service(value = "upcomingKitDestructionReminder")
public class UpcomingKitDestructionReminderTask implements Task {
  private final SexualAssaultKitRepository sexualAssaultKitRepository;
  private final EmailService emailService;

  public UpcomingKitDestructionReminderTask(SexualAssaultKitRepository sexualAssaultKitRepository, EmailService emailService) {
    this.sexualAssaultKitRepository = sexualAssaultKitRepository;
    this.emailService = emailService;
  }

  @Override
  public void run() {
    LocalDate sixMonthsFromNow = LocalDate.now().plusMonths(6);
    List<SexualAssaultKit> kitsToBeDestroyed = sexualAssaultKitRepository.findByLeDetailsPlannedDestructionDate(sixMonthsFromNow);
    kitsToBeDestroyed.stream()
      .filter(kit -> Boolean.TRUE.equals(kit.getLeDetails().getPlannedDestructionNotificationRequested()))
      .forEach(kit -> emailService.sendUpcomingKitDestructionEmail(kit));
  }
}
