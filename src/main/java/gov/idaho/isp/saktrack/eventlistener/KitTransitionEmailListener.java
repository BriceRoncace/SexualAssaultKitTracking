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
