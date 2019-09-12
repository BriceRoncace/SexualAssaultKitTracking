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

package gov.idaho.isp.saktrack;

import gov.idaho.isp.saktrack.task.Task;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;

@Configuration
@EnableScheduling
public class ScheduleConfig {
  private final Task kitsDueReminderTask;
  private final Task kitsOverdueNotificationTask;
  private final Task kitDestructionReminderTask;
  private final Task destructionDateNeededReminderTask;

  public ScheduleConfig(@Qualifier("kitsNeedLeActionReminder") Task kitsDueReminderTask, @Qualifier("kitsInViolationAtLeNotification") Task kitsOverdueNotificationTask, @Qualifier("upcomingKitDestructionReminder") Task kitDestructionReminderTask, @Qualifier("kitsNeedPlannedDestructionDateReminder") Task destructionDateNeededReminderTask) {
    this.kitsDueReminderTask = kitsDueReminderTask;
    this.kitsOverdueNotificationTask = kitsOverdueNotificationTask;
    this.kitDestructionReminderTask = kitDestructionReminderTask;
    this.destructionDateNeededReminderTask = destructionDateNeededReminderTask;
  }

  // cron pattern: sec, min, hr, day-of-month, month, day-of-week, year (optional)
  @Scheduled(cron="0 0 6 * * *")
  public void runDueTask() {
//    kitsDueReminderTask.run();
  }

  @Scheduled(cron="0 0 6 * * 1")
  public void runOverdueTask() {
//    kitsOverdueNotificationTask.run();
  }

  @Scheduled(cron="0 0 6 * * *")
  public void runDestructionReminderTask() {
//    kitDestructionReminderTask.run();
  }

  @Scheduled(cron="0 0 6 * * *")
  public void runDestructionDateNeededReminderTask() {
//  destructionDateNeededReminderTask.run();
  }
}