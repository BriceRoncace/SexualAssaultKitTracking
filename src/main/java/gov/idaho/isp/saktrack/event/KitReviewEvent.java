package gov.idaho.isp.saktrack.event;

import gov.idaho.isp.saktrack.SexualAssaultKit;
import gov.idaho.isp.saktrack.user.organization.LegalUser;
import java.time.LocalDateTime;

public class KitReviewEvent {
  private final LegalUser user;
  private final SexualAssaultKit kit;
  private final LocalDateTime timestamp;

  public KitReviewEvent(LegalUser user, SexualAssaultKit kit) {
    this.user = user;
    this.kit = kit;
    this.timestamp = LocalDateTime.now();
  }

  public LegalUser getUser() {
    return user;
  }

  public SexualAssaultKit getSexualAssaultKit() {
    return kit;
  }

  public LocalDateTime getTimestamp() {
    return timestamp;
  }
}
