package gov.idaho.isp.saktrack.event;

import gov.idaho.isp.saktrack.SexualAssaultKit;
import gov.idaho.isp.saktrack.user.organization.LawEnforcementUser;
import java.time.LocalDateTime;

public class KitReleasedForReviewEvent {
  private final LawEnforcementUser user;
  private final SexualAssaultKit kit;
  private final LocalDateTime timestamp;

  public KitReleasedForReviewEvent(LawEnforcementUser user, SexualAssaultKit kit) {
    this.user = user;
    this.kit = kit;
    this.timestamp = LocalDateTime.now();
  }

  public LawEnforcementUser getUser() {
    return user;
  }

  public SexualAssaultKit getSexualAssaultKit() {
    return kit;
  }

  public LocalDateTime getTimestamp() {
    return timestamp;
  }
}
