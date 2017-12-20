package gov.idaho.isp.saktrack.service;

import gov.idaho.isp.saktrack.SexualAssaultKit;
import gov.idaho.isp.saktrack.dto.EventDetails;
import gov.idaho.isp.saktrack.user.organization.OrganizationUser;
import gov.idaho.isp.saktrack.user.password.PasswordToken;
import java.util.List;

public interface EmailService {
  boolean sendPasswordResetEmail(PasswordToken token, OrganizationUser user);
  void sendKitsNeedLeActionReminderEmail(SexualAssaultKit kit);
  void sendKitPastSubmitEmail(SexualAssaultKit kit);
  void sendPlannedDestructionDateReminder(List<SexualAssaultKit> kits);
  void sendUpcomingKitDestructionEmail(SexualAssaultKit kit);
  void sendUserRegisteredEmail(OrganizationUser orgUser);
  void sendAttorneyNotificationEmail(SexualAssaultKit kit);
  void sendAttorneyHasReviewedNotificationEmail(SexualAssaultKit kit);
  void sendIncomingKitNotification(EventDetails details);
}
