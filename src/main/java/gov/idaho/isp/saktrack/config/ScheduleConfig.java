package gov.idaho.isp.saktrack.config;

import gov.idaho.isp.saktrack.task.Task;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;

@Configuration
@EnableScheduling
public class ScheduleConfig {
  private Task kitsNeedActionReminderTask;
  private Task kitsInViolationAtLeNotificationTask;
  private Task upcomingKitDestructionReminderTask;

  // cron pattern: sec, min, hr, day-of-month, month, day-of-week, year (optional)
  @Scheduled(cron="0 0 6 * * *")
  public void runKitsNeedActionReminderTask() {
//    kitsNeedActionReminderTask.run();
  }

  @Scheduled(cron="0 0 6 * * *")
  public void runUpcomingDestructionReminderTask() {
//    upcomingKitDestructionReminderTask.run();
  }

  @Scheduled(cron="0 0 6 * * 1")
  public void runKitsInViolationAtLeNotificationTask() {
//    kitsInViolationAtLeNotificationTask.run();
  }

  @Autowired @Qualifier("kitsNeedLeActionReminder")
  public void setKitsNeedActionReminderTask(Task kitsNeedActionReminderTask) {
    this.kitsNeedActionReminderTask = kitsNeedActionReminderTask;
  }

  @Autowired @Qualifier("kitsInViolationAtLeNotification")
  public void setKitsInViolationAtLeNotificationTask(Task kitsInViolationAtLeNotificationTask) {
    this.kitsInViolationAtLeNotificationTask = kitsInViolationAtLeNotificationTask;
  }

  @Autowired @Qualifier("upcomingKitDestructionReminder")
  public void setUpcomingKitDestructionReminderTask(Task upcomingKitDestructionReminderTask) {
    this.upcomingKitDestructionReminderTask = upcomingKitDestructionReminderTask;
  }
}