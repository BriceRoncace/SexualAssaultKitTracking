package gov.idaho.isp.saktrack.domain.dto;

import gov.idaho.isp.saktrack.validation.LocalDatePast;
import gov.idaho.isp.saktrack.validation.Range;
import gov.idaho.isp.saktrack.validation.group.Batch;
import gov.idaho.isp.saktrack.validation.group.Single;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;

public class EventDetails {
  @NotNull(message = "{org.id.null}")
  private Long orgId;

  @NotBlank(message = "{serialNumbers.blank}")
  @Range(message = "{serialNumbers.invalid.range}", groups = {Batch.class})
  @Pattern(regexp = "(\\s*?\\d+\\s*?)+?", message = "{serial.not.number}", groups = {Single.class})
  private String serialNumbers;

  private List<String> serialNumberList = new ArrayList<>();

  @LocalDatePast
  private LocalDate eventDate;

  private String notes;

  public Long getOrgId() {
    return orgId;
  }

  public void setOrgId(Long orgId) {
    this.orgId = orgId;
  }

  public String getSerialNumber() {
    return serialNumbers;
  }

  public void setSerialNumber(String serialNumber) {
    this.serialNumbers = serialNumber;
  }

  public String getSerialNumbers() {
    return serialNumbers;
  }

  public void setSerialNumbers(String serialNumbers) {
    this.serialNumbers = serialNumbers;
  }

  public String getSerialNumbersForMessage() {
    return serialNumberList.toString().replaceAll("\\[|\\]", "");
  }

  public List<String> getSerialNumberList() {
    return serialNumberList;
  }

  public void setSerialNumberList(List<String> serialNumberList) {
    this.serialNumberList = serialNumberList;
  }

  public LocalDate getEventDate() {
    return eventDate;
  }

  public void setEventDate(LocalDate eventDate) {
    this.eventDate = eventDate;
  }

  public String getNotes() {
    return notes;
  }

  public void setNotes(String notes) {
    this.notes = notes;
  }

  @Override
  public String toString() {
    return "EventDetails{" + "orgId=" + orgId + ", serialNumbers=" + serialNumbers + ", serialNumberList=" + serialNumberList + ", eventDate=" + eventDate + ", notes=" + notes + '}';
  }
}