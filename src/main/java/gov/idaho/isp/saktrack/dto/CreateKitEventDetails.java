package gov.idaho.isp.saktrack.dto;

import java.time.LocalDate;
import javax.validation.constraints.NotNull;

public class CreateKitEventDetails extends EventDetails {
  @NotNull(message = "{expirationDate.null}")
  private LocalDate expirationDate;

  public LocalDate getExpirationDate() {
    return expirationDate;
  }

  public void setExpirationDate(LocalDate expirationDate) {
    this.expirationDate = expirationDate;
  }
}
