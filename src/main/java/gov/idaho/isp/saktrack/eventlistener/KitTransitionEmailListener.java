package gov.idaho.isp.saktrack.eventlistener;

import gov.idaho.isp.saktrack.event.KitReleasedForReviewEvent;
import gov.idaho.isp.saktrack.event.KitReviewEvent;
import gov.idaho.isp.saktrack.event.KitSendEvent;
import gov.idaho.isp.saktrack.service.email.EmailService;
import org.springframework.context.event.EventListener;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Component;

@Component
public class KitTransitionEmailListener {
  private final EmailService emailService;

  public KitTransitionEmailListener(EmailService emailService) {
    this.emailService = emailService;
  }

  @EventListener @Async
  public void onSend(KitSendEvent e) {
    emailService.sendIncomingKitNotification(e.getEventDetails());
  }

  @EventListener @Async
  public void onReleasedForReview(KitReleasedForReviewEvent e) {
    emailService.sendAttorneyNotificationEmail(e.getSexualAssaultKit());
  }

  @EventListener @Async
  public void onReview(KitReviewEvent e) {
    emailService.sendAttorneyHasReviewedNotificationEmail(e.getSexualAssaultKit());
  }
}
