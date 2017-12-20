package gov.idaho.isp.saktrack;

import gov.idaho.isp.saktrack.organization.Organization;
import gov.idaho.isp.saktrack.persistence.listener.LastModifiedEntityListener;
import gov.idaho.isp.saktrack.persistence.listener.SexualAssaultKitAware;
import gov.idaho.isp.saktrack.validation.LocalDatePast;
import gov.idaho.isp.saktrack.validation.group.SendKit;
import java.io.Serializable;
import java.time.LocalDate;
import javax.persistence.Entity;
import javax.persistence.EntityListeners;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToOne;
import javax.validation.constraints.NotNull;
import org.hibernate.validator.constraints.NotBlank;

@Entity
@EntityListeners(LastModifiedEntityListener.class)
public class LabDetails implements SexualAssaultKitAware, Serializable {
  @Id
  @GeneratedValue(strategy=GenerationType.AUTO)
  private Long id;

  @ManyToOne
  @JoinColumn(name = "requestingOrganizationId")
  @NotNull(message = "{org.id.null}", groups = {SendKit.class})
  private Organization requestingLeAgency;

  @NotBlank(message = "{caseNumber.blank}", groups = {SendKit.class})
  private String caseNumber;

  @LocalDatePast(message = "{dateCompleted.in.future}")
  @NotNull(message = "{dateCompleted.null}", groups = {SendKit.class})
  private LocalDate dateCompleted;

  @Enumerated(EnumType.STRING)
  @NotNull(message = "{dnaDatabaseEntry.null}", groups = {SendKit.class})
  private YesNoNa dnaDatabaseEntry;

  @LocalDatePast(message = "{dnaDatabaseEntryDate.in.future}")
  private LocalDate dnaDatabaseEntryDate;

  @LocalDatePast(message = "{dnaDatabaseHitDate.in.future}")
  private LocalDate dnaDatabaseHitDate;

  @LocalDatePast(message = "{expungedDate.in.future}")
  private LocalDate expungedDate;

  @OneToOne(mappedBy = "labDetails")
  private SexualAssaultKit sexualAssaultKit;

  public boolean hasBeenAnalyzedAtLab() {
    return dateCompleted != null;
  }

  public Long getId() {
    return id;
  }

  public void setId(Long id) {
    this.id = id;
  }

  public Organization getRequestingLeAgency() {
    return requestingLeAgency;
  }

  public void setRequestingLeAgency(Organization requestingLeAgency) {
    this.requestingLeAgency = requestingLeAgency;
  }

  public String getCaseNumber() {
    return caseNumber;
  }

  public void setCaseNumber(String caseNumber) {
    this.caseNumber = caseNumber;
  }

  public LocalDate getDateCompleted() {
    return dateCompleted;
  }

  public void setDateCompleted(LocalDate dateCompleted) {
    this.dateCompleted = dateCompleted;
  }

  public YesNoNa getDnaDatabaseEntry() {
    return dnaDatabaseEntry;
  }

  public void setDnaDatabaseEntry(YesNoNa dnaDatabaseEntry) {
    this.dnaDatabaseEntry = dnaDatabaseEntry;
  }

  public LocalDate getDnaDatabaseEntryDate() {
    return dnaDatabaseEntryDate;
  }

  public void setDnaDatabaseEntryDate(LocalDate dnaDatabaseEntryDate) {
    this.dnaDatabaseEntryDate = dnaDatabaseEntryDate;
  }

  public LocalDate getDnaDatabaseHitDate() {
    return dnaDatabaseHitDate;
  }

  public void setDnaDatabaseHitDate(LocalDate dnaDatabaseHitDate) {
    this.dnaDatabaseHitDate = dnaDatabaseHitDate;
  }

  public LocalDate getExpungedDate() {
    return expungedDate;
  }

  public void setExpungedDate(LocalDate expungedDate) {
    this.expungedDate = expungedDate;
  }

  @Override
  public SexualAssaultKit getSexualAssaultKit() {
    return sexualAssaultKit;
  }

  public void setSexualAssaultKit(SexualAssaultKit sexualAssaultKit) {
    this.sexualAssaultKit = sexualAssaultKit;
  }

  @Override
  public String toString() {
    return "LabDetails{" + "id=" + id + ", requestingLeAgency=" + requestingLeAgency + ", caseNumber=" + caseNumber + ", dateCompleted=" + dateCompleted + ", dnaDatabaseEntry=" + dnaDatabaseEntry + ", dnaDatabaseEntryDate=" + dnaDatabaseEntryDate + ", dnaDatabaseHitDate=" + dnaDatabaseHitDate + ", expungedDate=" + expungedDate + '}';
  }
}
