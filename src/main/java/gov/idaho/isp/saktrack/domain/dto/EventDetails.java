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

package gov.idaho.isp.saktrack.domain.dto;

import gov.idaho.isp.saktrack.validation.LocalDatePast;
import gov.idaho.isp.saktrack.validation.Range;
import gov.idaho.isp.saktrack.validation.group.Batch;
import gov.idaho.isp.saktrack.validation.group.Single;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Pattern;

public class EventDetails {
  @NotNull(message = "{org.id.null}")
  private Long orgId;

  @NotBlank(message = "{serialNumbers.blank}")
  @Range(message = "{serialNumbers.invalid.range}", groups = {Batch.class})
  @Pattern(regexp = "(\\s*?\\d+\\s*?)+?", message = "{serial.not.number}", groups = {Single.class})
  private String serialNumbers;

  private List<String> serialNumberList = new ArrayList<>();

  private boolean ignoreUnknownKits;

  private List<String> unknownSerialNumberList = new ArrayList<>();

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
    List<String> serials = new ArrayList<>(serialNumberList);
    if (ignoreUnknownKits) {
      serials.removeAll(unknownSerialNumberList);
    }

    return serials.toString().replaceAll("\\[|\\]", "");
  }

  public List<String> getSerialNumberList() {
    return serialNumberList;
  }

  public void setSerialNumberList(List<String> serialNumberList) {
    this.serialNumberList = serialNumberList;
  }

  public boolean isIgnoreUnknownKits() {
    return ignoreUnknownKits;
  }

  public void setIgnoreUnknownKits(boolean ignoreUnknownKits) {
    this.ignoreUnknownKits = ignoreUnknownKits;
  }

  public List<String> getUnknownSerialNumberList() {
    return unknownSerialNumberList;
  }

  public void setUnknownSerialNumberList(List<String> unknownSerialNumberList) {
    this.unknownSerialNumberList = unknownSerialNumberList;
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
    return "EventDetails{" + "orgId=" + orgId + ", serialNumbers=" + serialNumbers + ", serialNumberList=" + serialNumberList + ", ignoreUnknownKits=" + ignoreUnknownKits + ", eventDate=" + eventDate + ", notes=" + notes + '}';
  }
}
