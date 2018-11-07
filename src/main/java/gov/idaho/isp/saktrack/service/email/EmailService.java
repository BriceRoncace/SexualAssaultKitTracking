package gov.idaho.isp.saktrack.service.email;

import gov.idaho.isp.saktrack.domain.SexualAssaultKit;
import gov.idaho.isp.saktrack.domain.dto.EventDetails;
import gov.idaho.isp.saktrack.domain.user.User;
import gov.idaho.isp.saktrack.domain.user.organization.OrganizationUser;
import gov.idaho.isp.saktrack.domain.user.password.PasswordToken;
import java.util.List;

public interface EmailService {
  boolean sendPasswordResetEmail(PasswordToken token, User user);
  void sendKitsNeedLeActionReminderEmail(SexualAssaultKit kit);
  void sendKitPastSubmitEmail(SexualAssaultKit kit);
  void sendPlannedDestructionDateReminder(List<SexualAssaultKit> kits);
  void sendUpcomingKitDestructionEmail(SexualAssaultKit kit);
  void sendUserRegisteredEmail(OrganizationUser orgUser);
  void sendAttorneyNotificationEmail(SexualAssaultKit kit);
  void sendAttorneyHasReviewedNotificationEmail(SexualAssaultKit kit);
  void sendIncomingKitNotification(EventDetails details);
}
